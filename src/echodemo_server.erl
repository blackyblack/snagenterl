-module(echodemo_server).

-behaviour(gen_server).

-export([start_link/0, process_req/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {permanentflag, daemonid, ppid, name, numsent, numrecv, nxtaddr, servicenxtaddr, myid,
                connectaddr, bindaddr, timeout, sleepmillis,
                snetsocket, agentsocket, allowremote, registeredflag,
                alivetimer}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_timestamp() ->
  {Mega, Sec, Micro} = os:timestamp(),
  (Mega*1000000 + Sec)*1000 + round(Micro/1000).

snetreply(Data, State) ->
  error_logger:info_msg("Send: ~p~n", [Data]),
  Socket = State#state.snetsocket,
  Numsent = State#state.numsent,
  ExtData = string:concat(Data, [0]),
  ok = enm:send(Socket, ExtData),
  NewState = State#state{numsent = Numsent + 1},
  {ok, NewState}.

main([MainPath, PermArg, DaemonIdArg, JsonArgs, ParentIdArg]) ->
  PermFlag = (PermArg == "1"),
  {DaemonId, _} = string:to_integer(DaemonIdArg),
  {ParentId, _} = string:to_integer(ParentIdArg),
  error_logger:info_msg("Args: Permanent = ~p, Daemon = ~p, Parent = ~p~n", [PermFlag, DaemonId, ParentId]),
  error_logger:info_msg("JSON: ~p~n", [list_to_binary(JsonArgs)]),
  %% expect a stringified version of JSON
  {ok, [{string,_,JsonArgsParsed}],_}=erl_scan:string(JsonArgs),
  RandomBytes = procutils:randombytes(8),
  MyId = binary:decode_unsigned(RandomBytes),
  JsonData = jsx:decode(list_to_binary(JsonArgsParsed), [return_maps]),
  SnAddress = lists:flatten(io_lib:format("ipc://~s/SuperNET.agents", [MainPath])),
  ListenAddress = lists:flatten(io_lib:format("ipc://~s/~p", [MainPath, DaemonId])),
  error_logger:info_msg("Json: ~p~n", [JsonData]),
  {ok, #state{
    permanentflag = PermFlag,
    daemonid = DaemonId,
    ppid = ParentId,
    name = "echodemo",
    numsent = 0,
    numrecv = 0,
    nxtaddr = binary_to_integer(maps:get(<<"NXT">>, JsonData, <<"0">>)),
    servicenxtaddr = binary_to_integer(maps:get(<<"serviceNXT">>, JsonData, <<"0">>)),
    myid = MyId,
    connectaddr = SnAddress,
    bindaddr = ListenAddress,
    timeout = maps:get(<<"timeout">>, JsonData, 0),
    sleepmillis = maps:get(<<"sleepmillis">>, JsonData, 100),
    allowremote = 1,
    registeredflag = 0}}.

%% gen_server callbacks

init([]) ->
  {ok, State} = main(application:get_env(echodemo, args, undefined)),
  error_logger:info_msg("State: ~p~n", [State]),
  {ok, Socket1} = enm:push([{connect, State#state.connectaddr}, list]),
  {ok, Socket2} = enm:bus([{bind, State#state.bindaddr}, {active, true}]),
  error_logger:info_msg("Connected on ~s~n", [State#state.connectaddr]),
  error_logger:info_msg("Listen on ~s~n", [State#state.bindaddr]),

  %% TODO: create register JSON and send to Push socket
  NewState = State#state{snetsocket = Socket1, agentsocket = Socket2},
  RegData = #{
    <<"daemonid">> => list_to_binary(integer_to_list(State#state.daemonid)),
    <<"methods">> => [<<"echo">>],
    <<"authmethods">> => [<<"echo">>],
    <<"pubmethods">> => [<<"echo">>],
    <<"pluginrequest">> => <<"SuperNET">>,
    <<"requestType">> => <<"register">>,
    <<"plugin">> => list_to_binary(State#state.name),
    <<"permanentflag">> => State#state.permanentflag,
    <<"sent">> => State#state.numsent,
    <<"recv">> => State#state.numrecv,
    <<"NXT">> => list_to_binary(integer_to_list(State#state.nxtaddr)),
    <<"serviceNXT">> => list_to_binary(integer_to_list(State#state.servicenxtaddr)),
    <<"endpoint">> => list_to_binary(State#state.bindaddr),
    <<"myid">> => list_to_binary(integer_to_list(State#state.myid)),
    <<"allowremote">> => State#state.allowremote,
    <<"millis">> => get_timestamp(),
    <<"sleepmillis">> => State#state.sleepmillis
  },
  JsonStr = jsx:encode(RegData),
  {ok, NewState2} = snetreply(binary_to_list(JsonStr), NewState),
  Timer = erlang:send_after(State#state.sleepmillis, self(), isalive, []),
  NewState3 = NewState2#state{alivetimer = Timer},
  {ok, NewState3}.

handle_call(Request, From, State) ->
    error_logger:info_msg("echodemo call ~p from ~p~n", [Request, From]),
    {noreply, State}.

handle_cast(Msg, State) ->
    error_logger:info_msg("echodemo cast ~p~n", [Msg]),
    {noreply, State}.

handle_info({_, {error, Msg}}, State) ->
    error_logger:info_msg("echodemo error: ~p. Stopping... ~n", [Msg]),
    {stop, {error, Msg}, State};

handle_info(isalive, State) ->
    erlang:cancel_timer(State#state.alivetimer),
    Alive = procutils:os_pingpid(State#state.ppid),
    if
        Alive /= 0 ->
	    error_logger:info_msg("Parent ~p died. Terminating.~n", [State#state.ppid]),
            {stop, normal, State};
        true ->
            Timer = erlang:send_after(State#state.sleepmillis, self(), isalive, []),
            NewState = State#state{alivetimer = Timer},
            {noreply, NewState}
    end;

handle_info({nnbus, _Socket, DataRaw}, State) ->
    RawSize = size(DataRaw),
    Data = binary:part(DataRaw, 0, RawSize - 1),
    error_logger:info_msg("nn_recv ~s~n", [binary_to_list(Data)]),
    Json = jsx:decode(Data, [return_maps]),
    error_logger:info_msg("json = ~p~n", [Json]),
    PluginStr = maps:get(<<"plugin">>, Json, <<"">>),
    AgentStr = maps:get(<<"agent">>, Json, <<"">>),
    DestPluginStr = maps:get(<<"destplugin">>, Json, <<"">>),
    DestAgentStr = maps:get(<<"destagent">>, Json, <<"">>),
    if
        (PluginStr == State#state.name) or (AgentStr == State#state.name) or
        (DestPluginStr == State#state.name) or (DestAgentStr == State#state.name) ->
            Answer = process_req(Json, State),
            process_reply(Answer, Json);
        true -> {noreply, State}
    end;

handle_info(Info, State) ->
    error_logger:info_msg("echodemo info ~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) -> init:stop(), ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% process good and bad responses from agent
process_reply({ok, Data, State}, _Req) ->
    JsonStr = jsx:encode(Data),
    {ok, NewState} = snetreply(binary_to_list(JsonStr), State),
    {noreply, NewState};

process_reply({error, _Reason, State}, Req) ->
    TagStr = maps:get(<<"tag">>, Req, nil),
    Answer = #{
        <<"result">> => <<"no response">>,
        <<"tag">> => TagStr,
        %% stdfields
        <<"allowremote">> => State#state.allowremote,
        <<"daemonid">> => list_to_binary(integer_to_list(State#state.daemonid)),
        <<"NXT">> => list_to_binary(integer_to_list(State#state.nxtaddr)),
        <<"serviceNXT">> => list_to_binary(integer_to_list(State#state.servicenxtaddr)),
        <<"myid">> => list_to_binary(integer_to_list(State#state.myid))
    },
    JsonStr = jsx:encode(Answer),
    {ok, NewState} = snetreply(binary_to_list(JsonStr), State),
    {noreply, NewState}.

%% move to specific agent implementation
process_req(_Json, State) ->
    error_logger:info_msg("processing agent request~n", []),
    Reply = #{
      <<"result">> => <<"ok">>
    },
    {ok, Reply, State}.
