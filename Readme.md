SuperNet erlang agent

How to build

Install rebar: https://github.com/basho/rebar
Change dict() to dict:dict() in rebar_config.erl in case of build errors.

Linux:

- Copy libnanomsg.a from btcd/libjl777/nanomsg/.libs to c_src
- Run compile.sh

Windows:

- Make sure Visual Studio is installed
- Download and build nanomsg as static library: https://github.com/nanomsg/nanomsg
- Copy nanomsg.lib to c_src_win/
- Copy ei.lib and erl_interface.lib from ERLANG_DIR/usr/lib to c_src_win/
- Run compile_native_win32.bat
- Copy enm_drv.dll to priv/

Or use prebuilt libraries from prebuilt/win32/
