#!/bin/sh
DIR="$( dirname $0 )"
cd $DIR
MY_NODENAME=${MY_NODENAME-snagenterl}

erl -pa ./ebin \
-config app.config \
-boot start_sasl \
-sname $MY_NODENAME \
-run echodemo_app run $@ \
-run init stop