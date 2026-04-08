# serve-managed.R
#
# Managed worker script for Docker deployment of phantasus.
#
# This script is Docker/Linux operational infrastructure; it does not
# belong in the R package API.  It lives in inst/ so it is shipped with
# the package and can be located via system.file().
#
# Architecture:
#   Parent process  — runs setup once, forks N workers, monitors them.
#   Worker process  — serves HTTP via httpuv; self-exits when R's full
#                     gc fires or max age is exceeded.
#
# The parent builds the app object once before any fork.
# All workers share it copy-on-write.
#
# Configuration (environment variables):
#   PHANTASUS_MAX_WORKER_AGE_S   default 21600 (6 hours)

suppressPackageStartupMessages({
    library(phantasus)
    library(httpuv)
    library(opencpu)
    library(futile.logger)
})

# ── configuration ─────────────────────────────────────────────────────────────

`%||%` <- function(a, b) if (length(a) > 0L && !is.null(a)) a else b

ports <- unlist(getPhantasusConf("internal_ports"))

MAX_AGE <- as.numeric(
    Sys.getenv("PHANTASUS_MAX_WORKER_AGE_S", unset = "21600")
)

flog.info("ports=%s  max_age=%.0f s",
          paste(range(ports), collapse = "-"), MAX_AGE)

# ── pre-fork setup ────────────────────────────────────────────────────────────
#
# Runs exactly once in the parent.  The resulting `app` object and all
# its closures are shared read-only across workers via CoW.

host <- getPhantasusConf("host")

app <- phantasus:::buildPhantasusApp(run_worker = NULL)
gc()   # compact heap before forking so CoW pages start clean

# ── shared helpers ────────────────────────────────────────────────────────────

get_pss_mb <- function(pid) {
    path  <- sprintf("/proc/%d/smaps_rollup", as.integer(pid))
    lines <- tryCatch(readLines(path), error = function(e) character(0))
    hit   <- grep("^Pss:", lines, value = TRUE)
    if (length(hit) == 0L) {
        return(NA_real_)
    }
    as.numeric(sub("^Pss:\\s+(\\d+).*", "\\1", hit)) / 1024
}

is_alive <- function(pid) {
    # pskill(pid, 0) returns TRUE even for zombie processes.
    # Read /proc/<pid>/status and check the State field instead.
    lines <- tryCatch(
        readLines(sprintf("/proc/%d/status", as.integer(pid))),
        error = function(e) character(0)
    )
    if (length(lines) == 0L) {
        return(FALSE)
    }
    state <- grep("^State:", lines, value = TRUE)
    length(state) > 0L && !grepl("Z", state[[1L]])
}

# ── worker body ───────────────────────────────────────────────────────────────

run_managed_worker <- function(port) {
    worker_born <- proc.time()[["elapsed"]]
    max_age     <- MAX_AGE * runif(1L, min = 1, max = 2)

    # Sentinel: fires only on a full (level 2) gc.
    # Two minor gc passes age it into generation 2 before we drop the
    # reference, so the finalizer only triggers on a full collection.
    gc_fired <- FALSE
    sentinel <- new.env(parent = emptyenv())
    reg.finalizer(sentinel, function(e) { gc_fired <<- TRUE }, onexit = FALSE)
    gc(full = FALSE)
    gc(full = FALSE)
    rm(sentinel)

    server <- tryCatch(
        startServer(host, port, app),
        error = function(e) {
            flog.error("worker port=%d pid=%d FAILED to start: %s",
                       port, Sys.getpid(), conditionMessage(e))
            NULL
        }
    )

    if (is.null(server)) {
        return(invisible(NULL))
    }

    flog.info("worker port=%d pid=%d started", port, Sys.getpid())

    # Service loop: check gc sentinel and age every 5000 ms.
    reason <- "unknown"
    repeat {
        httpuv::service(5000L)

        age_s <- proc.time()[["elapsed"]] - worker_born
        if (age_s > max_age) {
            reason <- sprintf("max age (%.0f s)", age_s)
            break
        }

        if (gc_fired) {
            reason <- sprintf("gc fired (age=%.0f s)", age_s)
            break
        }
    }

    flog.info("worker port=%d pid=%d exiting: %s", port, Sys.getpid(), reason)
    stopServer(server)
    # Return rather than quit() — quit() triggers parallel's CleanupParallel
    # hook in the parent via R's atexit mechanism, setting
    # R_interrupts_pending and silently breaking the parent's monitor loop.
    return(invisible(NULL))
}

# ── manager side ──────────────────────────────────────────────────────────────

jobs      <- list()   # mcparallel job objects, keyed by as.character(pid)
job_ports <- list()   # port number, keyed by as.character(pid)

start_worker <- function(port) {
    job <- parallel::mcparallel(run_managed_worker(port), detach = FALSE)
    pid <- job$pid
    jobs[[as.character(pid)]]      <<- job
    job_ports[[as.character(pid)]] <<- port
}

# ── entry point ───────────────────────────────────────────────────────────────

flog.info("starting %d workers on ports %d-%d",
          length(ports), min(ports), max(ports))

for (p in ports) {
    start_worker(p)
}

# shutdown: SIGTERM all live workers and exit the manager.
shutdown <- function() {
    flog.fatal("manager shutting down — terminating workers")
    for (pid_char in names(jobs)) {
        if (is_alive(as.integer(pid_char))) {
            tryCatch(
                tools::pskill(as.integer(pid_char), tools::SIGTERM),
                error = function(e) NULL
            )
        }
    }
    quit(save = "no", status = 0L)
}

# Manager gc sentinel: logs when a full (level 2) gc fires in the manager.
# Reset after each detection so we catch subsequent collections too.
mgr_gc_fired <- FALSE
mgr_sentinel <- new.env(parent = emptyenv())
reg.finalizer(mgr_sentinel,
              function(e) { mgr_gc_fired <<- TRUE },
              onexit = FALSE)
gc(full = FALSE)
gc(full = FALSE)
rm(mgr_sentinel)

# Respawn loop.
#
# mccollect(wait=FALSE, timeout=300) blocks until any single worker
# finishes (or 300 s elapse), then returns whatever has completed.
#
# Distinguish SIGINT from SIGCHLD: if interrupted and no job finished,
# check whether any known pid is actually dead.  If none → genuine SIGINT
# → shut down.

repeat {
    interrupted <- FALSE
    finished <- tryCatch(
        parallel::mccollect(jobs, wait = FALSE, timeout = 300L),
        interrupt = function(e) { interrupted <<- TRUE; NULL }
    )
    if (is.null(finished)) {
        finished <- list()
    }

    mgr_pss   <- get_pss_mb(Sys.getpid()) %||% 0
    wkr_pss   <- vapply(names(job_ports), function(pid_char) {
        get_pss_mb(as.integer(pid_char)) %||% 0
    }, numeric(1L))
    total_pss <- sum(c(mgr_pss, wkr_pss), na.rm = TRUE)
    flog.info("manager loop  total_pss=%.1f MB", total_pss)

    if (mgr_gc_fired) {
        flog.warn("full gc fired in manager")
        mgr_gc_fired <- FALSE
        mgr_sentinel <- new.env(parent = emptyenv())
        reg.finalizer(mgr_sentinel,
                      function(e) { mgr_gc_fired <<- TRUE },
                      onexit = FALSE)
        gc(full = FALSE)
        gc(full = FALSE)
        rm(mgr_sentinel)
    }

    if (interrupted && length(finished) == 0L) {
        # Distinguish SIGINT (no worker actually dead) from a SIGCHLD that
        # mccollect hasn't collected yet.
        any_dead <- any(vapply(names(jobs), function(pid_char) {
            !is_alive(as.integer(pid_char))
        }, logical(1L)))

        if (!any_dead) {
            shutdown()
        }

        # A worker is dead but mccollect hasn't collected it yet.
        # Give the zombie a moment to be reaped, then retry.
        finished <- parallel::mccollect(jobs, wait = FALSE, timeout = 0.1)
        if (is.null(finished)) {
            finished <- list()
        }
    }

    for (pid_char in names(finished)) {
        port <- job_ports[[pid_char]]
        jobs[[pid_char]]      <- NULL
        job_ports[[pid_char]] <- NULL
        flog.info("manager port=%d pid=%s exited — replacing", port, pid_char)
        start_worker(port)
    }
}
