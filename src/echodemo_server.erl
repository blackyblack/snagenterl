-module(echodemo_server).

-behaviour(gen_server).

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {permanentflag, daemonid, ppid, name, numsent, numrecv, nxtaddr, servicenxtaddr, myid,
                connectaddr, bindaddr, timeout, sleepmillis,
                snetsocket, agentsocket}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

snetreply(Data, State) ->
  error_logger:info_msg("Send: ~p~n", [Data]),
  Socket = State#state.snetsocket,
  Numsent = State#state.numsent,
  ExtData = string:concat(Data, [0]),
  ok = enm:send(Socket, ExtData),
  NewState = State#state{numsent = Numsent + 1},
  {ok, NewState}.

main([PermArg, DaemonIdArg, JsonArgs, ParentIdArg]) ->
  PermFlag = (PermArg == "1"),
  {DaemonId, _} = string:to_integer(DaemonIdArg),
  {ParentId, _} = string:to_integer(ParentIdArg),
  error_logger:info_msg("Args: Permanent = ~p, Daemon = ~p, Parent = ~p~n", [PermFlag, DaemonId, ParentId]),
  error_logger:info_msg("JSON: ~p~n", [list_to_binary(JsonArgs)]),
  MyId = 999,
  JsonData = jsx:decode(list_to_binary(JsonArgs), [return_maps]),
  error_logger:info_msg("Json: ~p~n", [JsonData]),
  {ok, #state{
    permanentflag = PermFlag,
    daemonid = DaemonId,
    ppid = ParentId,
    name = "echodemo",
    numsent = 0,
    numrecv = 0,
    nxtaddr=binary_to_integer(maps:get(<<"NXT">>, JsonData, <<"0">>)),
    servicenxtaddr=binary_to_integer(maps:get(<<"serviceNXT">>, JsonData, <<"0">>)),
    myid = MyId,
    connectaddr = "ipc://SuperNET.agents",
    bindaddr = string:concat("ipc://", integer_to_list(DaemonId)),
    timeout = maps:get(<<"timeout">>, JsonData, 0),
    sleepmillis = maps:get(<<"sleepmillis">>, JsonData, 100)}}.

%% gen_server callbacks

init([]) ->
  {ok, State} = main(application:get_env(echodemo, args, undefined)),
  error_logger:info_msg("State: ~p~n", [State]),
  {ok, Socket1} = enm:push([{connect, State#state.connectaddr}, list]),
  {ok, Socket2} = enm:bus([{bind, State#state.bindaddr}, {active, false}]),
  error_logger:info_msg("Connected on ~p~n", [State#state.connectaddr]),
  error_logger:info_msg("Listen on ~p~n", [State#state.bindaddr]),

  %% TODO: create register JSON and send to Push socket
  NewState = State#state{snetsocket = Socket1, agentsocket = Socket2},
  snetreply("test", NewState).

handle_call(Request, From, State) ->
    error_logger:info_msg("echodemo call ~p from ~p~n", [Request, From]),
    {noreply, State}.

handle_cast(Msg, State) ->
    error_logger:info_msg("echodemo cast ~p~n", [Msg]),
    {noreply, State}.

handle_info({_, {error, Msg}}, State) ->
    error_logger:info_msg("echodemo error: ~p. Stopping... ~n", [Msg]),
    {stop, {error, Msg}, State};

handle_info(Info, State) ->
    error_logger:info_msg("echodemo info ~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
