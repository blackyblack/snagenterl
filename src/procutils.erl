-module(procutils).
-export([os_get_ppid/0, os_pingpid/1, randombytes/1]).
-on_load(init/0).

init() ->
    PrivDir = case code:priv_dir(?MODULE) of
                  {error, bad_name} ->
                      EbinDir = filename:dirname(code:which(?MODULE)),
                      AppPath = filename:dirname(EbinDir),
                      filename:join(AppPath, "priv");
                  Path ->
                      Path
              end,
    NifPath = string:join([PrivDir, "procutils"], "/"),
    ok = erlang:load_nif(NifPath, 0).

os_get_ppid() ->
    exit(nif_library_not_loaded).
os_pingpid(_X) ->
    exit(nif_library_not_loaded).
randombytes(_X) ->
    exit(nif_library_not_loaded).