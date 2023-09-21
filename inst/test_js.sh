#!/bin/bash
R -e "devtools::install('.', upgrade=FALSE)"

mkdir -p inst/www/phantasus.js/jasmine/cache
cp "./inst/testdata/GSE27112-GPL6103.rda" inst/www/phantasus.js/jasmine/cache
cp -r ./inst/testdata/config inst/www/phantasus.js/jasmine/cache/
cd inst/www/phantasus.js/
npm install karma --save-dev
npm install
export R_USER_CONFIG_DIR=jasmine/cache/config
export R_CONFIG_ACTIVE=test_js

R -e "phantasus::getES('GSE53986')"

touch server.log
R -e "phantasus::servePhantasus()" 2>&1 | tee server.log &
PH_PID=$!


# Waiting for the server to start
while ! grep -q started server.log ; do sleep 0.1; done

sleep 0.1

./node_modules/karma/bin/karma start my.conf.js --single-run
RETVAL=$?

cd

function finish {
    kill $PH_PID
    echo Killed $PH_PID
    echo "Killed OpenCPU-server"
}
trap finish EXIT

if [ $RETVAL -eq 0 ]
then
    echo "Jasmine tests are successfully passed"
    exit 0
else
    echo "Tests failed" >&2
    exit 1
fi



