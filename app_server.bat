@echo off

set ARGS=%*

if "%MY_NODENAME%"=="" (
    set MY_NODENAME=snagenterl
)

erl ^
-pa %~dp0ebin ^
-config app.config ^
-boot start_sasl ^
-sname %MY_NODENAME% ^
-run echodemo_app run %ARGS%