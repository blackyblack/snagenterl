-module(snagent_app).

-behaviour(application).

%% API
-export([start/0, stop/0, run/0, run/1]).
%% Application callbacks
-export([start/2, stop/1]).

%% API

start() ->
  application:start(snagent).

stop() ->
  application:stop(snagent).

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
  application:set_env(snagent, args, Args),
  application:start(snagent).

  
%% Application callbacks

start(_StartType, _StartArgs) ->
  Args = application:get_env(snagent, args, undefined),
  error_logger:info_msg("Args = ~p~n", [Args]),
  enm:start_link(),
  snagent_sup:start_link().

stop(_State) ->
  enm:stop(),
  ok.