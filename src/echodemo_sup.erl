-module(echodemo_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).
%% Supervisor  callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, transient, 5000, Type, [I]}).

%% API

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% Supervisor callbacks

init([]) ->
  Ch1 = ?CHILD(echodemo_server, worker),
  {ok, {{one_for_one, 10, 10}, [Ch1]}}.