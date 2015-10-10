#!/bin/sh
DIR="$( pwd )"
BASEDIR="$( dirname $0 )"
cd $BASEDIR
MY_NODENAME=${MY_NODENAME-snagenterl}

erl -pa ./ebin \
-config app.config \
-boot start_sasl \
-sname $MY_NODENAME \
-noshell \
-run snagent_app run $DIR $@