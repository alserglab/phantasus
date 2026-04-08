#!/usr/bin/env bash
# test-serve-managed.sh
#
# Integration test for serve-managed.R + nginx.
#
# Tests the full stack: serve-managed.R workers + nginx upstream routing.
# Intended to run in the Vagrant VM or a Docker container on Linux.
# Not part of devtools::test().
#
# Exit code: 0 = all assertions passed, non-zero = first failure.
# Each step prints PASS/FAIL so failures are easy to locate.
#
# Usage:
#   bash phantasus/inst/test-serve-managed.sh

set -euo pipefail

# ── configuration ──────────────────────────────────────────────────────────────

N_WORKERS=3
BASE_PORT=8991
NGINX_PORT=8990
POLL_URL_PATH="phantasus/ocpu/library/base/R/Sys.time"
GC_URL_PATH="phantasus/ocpu/library/base/R/Sys.time"
NGINX_CONF_DIR=$(mktemp -d)
NGINX_LOG_DIR=$(mktemp -d)
NGINX_PID_FILE="$NGINX_CONF_DIR/nginx.pid"
CFG_DIR=$(mktemp -d)/R/phantasus
CACHE_ROOT=$(mktemp -d)
MANAGER_LOG=$(mktemp)
MANAGER_PID=""

PORTS=()
for i in $(seq 0 $((N_WORKERS - 1))); do
    PORTS+=($((BASE_PORT + i)))
done

# ── helpers ────────────────────────────────────────────────────────────────────

pass() { printf "PASS  %s\n" "$*"; }
fail() { printf "FAIL  %s\n" "$*"; cleanup; exit 1; }

cleanup() {
    set +e
    if [ -n "$MANAGER_PID" ] && kill -0 "$MANAGER_PID" 2>/dev/null; then
        kill -INT "$MANAGER_PID" 2>/dev/null
        sleep 2
        kill -TERM "$MANAGER_PID" 2>/dev/null
    fi
    nginx -s stop -c "$NGINX_CONF_DIR/nginx.conf" 2>/dev/null || true
    for p in "${PORTS[@]}"; do
        fuser -k "${p}/tcp" 2>/dev/null || true
    done
    fuser -k "${NGINX_PORT}/tcp" 2>/dev/null || true
}
trap cleanup EXIT

wait_http_200() {
    local url=$1 timeout_s=$2 label=${3:-"$url"}
    local deadline=$(( $(date +%s) + timeout_s ))
    while [ "$(date +%s)" -lt "$deadline" ]; do
        if curl -sf --max-time 3 "$url" >/dev/null 2>&1; then
            return 0
        fi
        sleep 1
    done
    fail "timeout waiting for HTTP 200: $label"
}

count_5xx() {
    local url=$1 n=$2
    local failures=0
    for i in $(seq 1 "$n"); do
        code=$(curl -s -o /dev/null -w "%{http_code}" --max-time 5 "$url" 2>/dev/null || echo "000")
        if [[ "$code" == 5* ]]; then
            failures=$((failures + 1))
        fi
    done
    echo "$failures"
}

# Return the pid of the R process listening on a port, or empty string.
pid_on_port() {
    local port=$1
    ss -tlnp "sport = :${port}" 2>/dev/null \
        | grep -oP 'pid=\K[0-9]+' \
        | head -1 || true
}

# ── setup ──────────────────────────────────────────────────────────────────────

echo "=== serve-managed integration test ==="
echo "workers=${N_WORKERS}  ports=${PORTS[*]}  nginx_port=${NGINX_PORT}"
echo ""

mkdir -p "$CFG_DIR"
mkdir -p "$CACHE_ROOT"/{geo,annotationdb,fgsea,counts}

{
    echo "default:"
    echo "  host: \"0.0.0.0\""
    echo "  preloaded_dir: NULL"
    echo "  static_root: \"\""
    echo "  cache_root: \"$CACHE_ROOT\""
    echo "  internal_ports:"
    for p in "${PORTS[@]}"; do
        echo "    - $p"
    done
    echo "  cache_folders:"
    echo "    geo_path: \"$CACHE_ROOT/geo\""
    echo "    annot_db: \"$CACHE_ROOT/annotationdb\""
    echo "    fgsea_pathways: \"$CACHE_ROOT/fgsea\""
    echo "    rnaseq_counts: \"$CACHE_ROOT/counts\""
} > "$CFG_DIR/user.conf"

SCRIPT=$(Rscript --vanilla -e \
    "cat(system.file('serve-managed.R', package='phantasus'))" 2>/dev/null)
if [ -z "$SCRIPT" ] || [ ! -f "$SCRIPT" ]; then
    fail "serve-managed.R not found via system.file() — is phantasus installed?"
fi

{
    echo "worker_processes 1;"
    echo "pid $NGINX_PID_FILE;"
    echo "error_log $NGINX_LOG_DIR/error.log warn;"
    echo "events { worker_connections 64; }"
    echo "http {"
    echo "    access_log $NGINX_LOG_DIR/access.log;"
    echo "    upstream phantasus_backend {"
    echo "        least_conn;"
    for p in "${PORTS[@]}"; do
        echo "        server localhost:$p;"
    done
    echo "    }"
    echo "    server {"
    echo "        listen $NGINX_PORT;"
    echo "        location / {"
    echo "            proxy_pass http://phantasus_backend;"
    echo "        }"
    echo "    }"
    echo "}"
} > "$NGINX_CONF_DIR/nginx.conf"

# ── step 1: start serve-managed.R ─────────────────────────────────────────────

echo "--- Step 1: start serve-managed.R ($N_WORKERS workers)"

R_USER_CONFIG_DIR=$(dirname "$(dirname "$CFG_DIR")") \
R_CONFIG_ACTIVE=default \
PHANTASUS_MAX_WORKER_AGE_S=10 \
    Rscript "$SCRIPT" > "$MANAGER_LOG" 2>&1 &
MANAGER_PID=$!

for p in "${PORTS[@]}"; do
    wait_http_200 "http://localhost:${p}/${POLL_URL_PATH}" 60 "port $p"
done
pass "all $N_WORKERS workers started and responding"

# ── step 2: start nginx ────────────────────────────────────────────────────────

echo "--- Step 2: start nginx"
nginx -c "$NGINX_CONF_DIR/nginx.conf"
wait_http_200 "http://localhost:${NGINX_PORT}/${POLL_URL_PATH}" 10 "nginx"
pass "nginx started and proxying to workers"

# ── step 3: 20 healthy requests through nginx ──────────────────────────────────

echo "--- Step 3: 20 requests through nginx (expect all 200)"
failures=$(count_5xx "http://localhost:${NGINX_PORT}/${POLL_URL_PATH}" 20)
if [ "$failures" -ne 0 ]; then
    fail "Step 3: $failures/20 requests returned 5xx before gc test"
fi
pass "Step 3: 0/20 requests failed (pre-gc baseline)"

# ── step 4: wait for max_age replacement on first worker port ─────────────────
#
# Workers are started with MAX_AGE=30 s (random factor 1-2x means exit
# within 30-60 s).  We poll the manager log for the replacement message
# and confirm the port comes back up.

echo "--- Step 4: wait for max_age replacement on port $BASE_PORT"
target_port=${PORTS[0]}
replaced=false
deadline=$(( $(date +%s) + 120 ))

while [ "$(date +%s)" -lt "$deadline" ]; do
    sleep 2
    if grep -q "port=${target_port}.*exited.*replacing\|exited.*replacing.*port=${target_port}" \
            "$MANAGER_LOG" 2>/dev/null; then
        replaced=true
        break
    fi
done

if [ "$replaced" = "false" ]; then
    fail "Step 4: no replacement seen on port $target_port within 120 s"
fi
pass "Step 4: max_age triggered replacement on port $target_port"

# ── step 5: wait for replacement worker ───────────────────────────────────────

echo "--- Step 5: wait for replacement worker on port $target_port"
wait_http_200 "http://localhost:${target_port}/${POLL_URL_PATH}" 60 \
    "replacement worker on port $target_port"
pass "Step 5: replacement worker on port $target_port is responding"

# ── step 6: 20 more requests confirm stable state ─────────────────────────────

echo "--- Step 6: 20 more requests post-replacement"
failures=$(count_5xx "http://localhost:${NGINX_PORT}/${POLL_URL_PATH}" 20)
if [ "$failures" -ne 0 ]; then
    fail "Step 6: $failures/20 requests failed after replacement"
fi
pass "Step 6: 0/20 requests failed post-replacement"

# ── step 7: SIGINT shutdown ────────────────────────────────────────────────────

echo "--- Step 7: SIGINT shutdown"
kill -INT "$MANAGER_PID"

deadline=$(( $(date +%s) + 15 ))
while [ "$(date +%s)" -lt "$deadline" ]; do
    if ! kill -0 "$MANAGER_PID" 2>/dev/null; then
        break
    fi
    sleep 1
done
if kill -0 "$MANAGER_PID" 2>/dev/null; then
    fail "Step 7: manager still alive 15s after SIGINT"
fi
pass "Step 7: manager exited after SIGINT"
MANAGER_PID=""

# Poll for up to 15 s — workers in httpuv::service() may take a few
# seconds to process SIGTERM and release their sockets.
ports_free=false
deadline=$(( $(date +%s) + 15 ))
while [ "$(date +%s)" -lt "$deadline" ]; do
    all_free=true
    for p in "${PORTS[@]}"; do
        if ss -tlnp "sport = :$p" | grep -q LISTEN; then
            all_free=false
            break
        fi
    done
    if [ "$all_free" = "true" ]; then
        ports_free=true
        break
    fi
    sleep 1
done
if [ "$ports_free" = "false" ]; then
    for p in "${PORTS[@]}"; do
        if ss -tlnp "sport = :$p" | grep -q LISTEN; then
            echo "  port $p still bound"
        fi
    done
    fail "Step 7: some worker ports still bound after shutdown"
fi
pass "Step 7: all worker ports freed after shutdown"

# ── summary ────────────────────────────────────────────────────────────────────

echo ""
echo "=== All steps passed ==="
