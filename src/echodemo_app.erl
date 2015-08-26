-module(echodemo_app).

-behaviour(application).

%% API
-export([start/0, stop/0, run/0, run/1]).
%% Application callbacks
-export([start/2, stop/1]).

%% API

start() ->
  application:start(echodemo).

stop() ->
  application:stop(echodemo).

run() ->
  error_logger:info_msg("No args passed. Terminating.~n", []),
  {error, noargs}.

run([]) ->
  error_logger:info_msg("No args passed. Terminating.~n", []),
  {error, noargs};

run([[]]) ->
  error_logger:info_msg("No args passed. Terminating.~n", []),
  {error, noargs};

run(Args) ->
  application:set_env(echodemo, args, Args),
  application:start(echodemo).

  
%% Application callbacks

start(_StartType, _StartArgs) ->
  error_logger:info_msg("Hello from echodemo.~n", []),
  Args = application:get_env(echodemo, args, undefined),
  error_logger:info_msg("Args = ~p~n", [Args]),
  enm:start_link(),
  echodemo_sup:start_link().

stop(_State) ->
  enm:stop(),
  ok.