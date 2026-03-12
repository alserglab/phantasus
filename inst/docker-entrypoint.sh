#!/bin/bash

RSCRIPT_PID=""

_term() {
  trap ERR
  echo "Caught signal!"
  nginx -s stop
  if [ -n "$RSCRIPT_PID" ]; then
    kill -2 "$RSCRIPT_PID" 2>/dev/null || true  # SIGINT → R calls q() → CleanupParallel kills children
  fi
}

_term_config(){
    echo "Failed to complete setup! Bad configuration file or user: $OCPU_USER doesn't have permissions to write cache or configuration file."
    exit 1
}

trap _term SIGINT SIGTERM

trap _term_config ERR
set -eE

chown -R $OCPU_USER /var/log/nginx
chown -R $OCPU_USER /var/lib/nginx
chown -R $OCPU_USER /var/cache/nginx

touch /run/nginx.pid
chown $OCPU_USER /run/nginx.pid

# Let's not change user files ownership?
# chown -R $OCPU_USER $R_USER_CONFIG_DIR/R/phantasus

gosu $OCPU_USER R -e "phantasus:::createDockerConf(); phantasus::setupPhantasus()" #|| _term_config

# Generate nginx upstream block from internal_ports in user config.
# Falls back to ports 8001-8010 if the key is absent.
UPSTREAM_PORTS=$(gosu $OCPU_USER Rscript --vanilla -e "
  tryCatch({
    ports <- unlist(phantasus::getPhantasusConf('internal_ports'))
    if (is.null(ports) || length(ports) == 0) stop('internal_ports not configured')
    cat(paste(ports, collapse=' '), '\n')
  }, error = function(e) {
    cat('8001 8002 8003 8004 8005 8006 8007 8008 8009 8010\n')
  })
" 2>/dev/null)

{
  echo "upstream phantasus_backend {"
  echo "    least_conn;"
  for p in $UPSTREAM_PORTS; do
    echo "    server localhost:$p;"
  done
  echo "}"
} > /etc/nginx/conf.d/upstream.conf

gosu $OCPU_USER nginx

gosu $OCPU_USER Rscript -e "
  ports <- unlist(phantasus::getPhantasusConf('internal_ports'))
  message('Starting ', length(ports), ' R server instances on ports ',
          paste(ports, collapse=', '))
  phantasus::servePhantasus(port = ports, openInBrowser = FALSE)
" &
RSCRIPT_PID=$!


color() {
    STARTCOLOR="\e[38;5;$2";
    ENDCOLOR="\e[0m";
    export "$1"="$STARTCOLOR%s"
}
color darkred 196m
color pinkish 211m
color blue 27m
color lightblue 68m
color coral 203m
color lessblue 26m
color normal 0m

printf $darkred "#### "
printf $pinkish "++++ "
printf $blue'\n' "----"
printf $darkred "#### "
printf $pinkish "++++ "
printf $blue'\n' "----"
printf $darkred "#### "
printf $pinkish "++++ "
printf $blue'\n' "----"

printf $lightblue "====      "
printf $coral "&&&&"
printf "\e[0m%b"'\n' "      _____    _                       _                                   _                         "
printf $lightblue "====      "
printf $coral "&&&&"
printf "\e[0m%s\n" "     |  __ \  | |                     | |                                 (_)                        "
printf $lightblue   "====      "
printf $coral"&&&&     "
printf "\e[0m%s\n" "| |__) | | |__     __ _   _ __   | |_    __ _   ___   _   _   ___     _   ___     _   _   _ __  "
printf $coral "&&&& "
printf $darkred "#### "
printf $lessblue "----     "
printf "\e[0m%s\n" "|  ___/  | '_ \   / _\` | | '_ \  | __|  / _\` | / __| | | | | / __|   | | / __|   | | | | | '_ \ "
printf $coral "&&&& "
printf $darkred "#### "
printf $lessblue "----     "
printf "\e[0m%s\n" "| |      | | | | | (_| | | | | | | |_  | (_| | \__ \ | |_| | \__ \   | | \__ \   | |_| | | |_) |"
printf $coral "&&&& "
printf $darkred "#### "
printf $lessblue "----     "
printf "\e[0m%s\n" "|_|      |_| |_|  \__,_| |_| |_|  \__|  \__,_| |___/  \__,_| |___/   |_| |___/    \__,_| | .__/ "
printf $blue "----"
printf "\e[0m%s\n" "                                                                                                        | |    "
printf $blue "----"
printf "\e[0m%s\n" "                                                                                                        |_|    "
printf $blue "----"
printf "\e[0m%s\n" ""


wait $RSCRIPT_PID
