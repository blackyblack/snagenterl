-module(echodemo_server).

-behaviour(gen_server).

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {permanentflag, daemonid, ppid, name, numsent, numrecv, nxtaddr, servicenxtaddr, myid,
                connectaddr, bindaddr, timeout, sleepmillis,
                snetsocket, agentsocket, allowremote, registeredflag,
                alivetimer}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_timestamp() ->
    {Mega, Sec, Micro} = os:timestamp(),
    (Mega*1000000 + Sec)*1000 + round(Micro/1000).

bit_is_set(X, {Result, Index, Bitset}) ->
    Check = 1 bsl Index,
    if
        Check band Bitset /= 0 -> {Result, Index + 1, Bitset};
        true -> {Result ++ [X], Index + 1, Bitset}
    end.

snetreply(Data, State) ->
  error_logger:info_msg("Send: ~p~n", [Data]),
  Socket = State#state.snetsocket,
  Numsent = State#state.numsent,
  ExtData = string:concat(Data, [0]),
  ok = enm:send(Socket, ExtData),
  NewState = State#state{numsent = Numsent + 1},
  {ok, NewState}.

create_agent([MainPath, PermArg, DaemonIdArg, JsonArgs, ParentIdArg]) ->
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

   State = #state{
    permanentflag = PermFlag,
    daemonid = DaemonId,
    ppid = ParentId,
    name = agent_name(),
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
    registeredflag = 0},

  %% add default settings for the agent
  {ok, _Answer, NewState} = agent_process_register(State, JsonData),
  %% get disabled methods mask
  agent_register(NewState, JsonData).

%% gen_server callbacks

init([]) ->
  {ok, State, DisabledMask} = create_agent(application:get_env(echodemo, args, undefined)),
  error_logger:info_msg("State: ~p~n", [State]),
  {ok, Socket1} = enm:push([{connect, State#state.connectaddr}, list]),
  {ok, Socket2} = enm:bus([{bind, State#state.bindaddr}, {active, true}]),
  error_logger:info_msg("Connected on ~s~n", [State#state.connectaddr]),
  error_logger:info_msg("Listen on ~s~n", [State#state.bindaddr]),

  MethodsRaw = agent_methods(),
  AuthMethodsRaw = agent_auth_methods(),
  PubMethodsRaw = agent_pub_methods(),

  %% filter based on disabled mask
  {Methods, _, _} = lists:foldl(fun(X, Acc) -> bit_is_set(X, Acc) end, {[], 0, DisabledMask}, MethodsRaw),
  {AuthMethods, _, _} = lists:foldl(fun(X, Acc) -> bit_is_set(X, Acc) end, {[], 0, DisabledMask}, AuthMethodsRaw),
  {PubMethods, _, _} = lists:foldl(fun(X, Acc) -> bit_is_set(X, Acc) end, {[], 0, DisabledMask}, PubMethodsRaw),

  NewState = State#state{snetsocket = Socket1, agentsocket = Socket2},
  RegData = #{
    <<"methods">> => Methods,
    <<"authmethods">> => AuthMethods,
    <<"pubmethods">> => PubMethods,
    <<"pluginrequest">> => <<"SuperNET">>,
    <<"requestType">> => <<"register">>,
    <<"plugin">> => State#state.name,
    <<"permanentflag">> => State#state.permanentflag,
    <<"sent">> => State#state.numsent,
    <<"recv">> => State#state.numrecv,
    <<"endpoint">> => list_to_binary(State#state.bindaddr),
    <<"millis">> => get_timestamp(),
    <<"sleepmillis">> => State#state.sleepmillis
  },
  {ok, NewState2} = reply_with_stdfields(RegData, NewState),

  %% start periodical timer to check parent is alive
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
            {ok, NewState} = agent_idle(State),
            Timer = erlang:send_after(NewState#state.sleepmillis, self(), isalive, []),
            NewState2 = NewState#state{alivetimer = Timer},
            {noreply, NewState2}
    end;

handle_info({nnbus, _Socket, DataRaw}, State) ->
    RawSize = size(DataRaw),
    Data = binary:part(DataRaw, 0, RawSize - 1),
    error_logger:info_msg("nn_recv ~s~n", [binary_to_list(Data)]),
    case jsx:is_json(Data) of
        true ->
            Json = jsx:decode(Data, [return_maps]),
            error_logger:info_msg("json = ~p~n", [Json]),
            {JsonReq, Token} = split_json_array(Json),
            if
                Token /= nil ->
                    Forwarder = maps:get(<<"forwarder">>, Token, <<"">>),
                    Sender = maps:get(<<"sender">>, Token, <<"">>),
                    Valid = maps:get(<<"valid">>, Token, 0),
                    error_logger:info_msg("token: forwarder = ~s, sender = ~s, valid = ~p~n", [Forwarder, Sender, Valid]);
                true -> ok
            end,
            PluginStr = maps:get(<<"plugin">>, JsonReq, <<"">>),
            AgentStr = maps:get(<<"agent">>, JsonReq, <<"">>),
            DestPluginStr = maps:get(<<"destplugin">>, JsonReq, <<"">>),
            DestAgentStr = maps:get(<<"destagent">>, JsonReq, <<"">>),
            TagStr = maps:get(<<"tag">>, JsonReq, nil),
            if
                (PluginStr == State#state.name) or (AgentStr == State#state.name) or
                (DestPluginStr == State#state.name) or (DestAgentStr == State#state.name) ->
                    Reply = agent_process_req(JsonReq, State),
                    Answer = process_reply(Reply, JsonReq, TagStr),
                    {ok, NewState} = reply_with_stdfields(Answer, State),
                    {noreply, NewState};
                true -> {noreply, State}
            end;
        false ->
            error_logger:info_msg("couldnt parse (~s)~n", [binary_to_list(Data)]),
            Answer = #{
                <<"result">> => <<"unparseable">>,
                <<"message">> => Data
            },
            {ok, NewState} = reply_with_stdfields(Answer, State),
            {noreply, NewState}
    end;

handle_info(Info, State) ->
    error_logger:info_msg("echodemo info ~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, State) -> agent_shutdown(State, 0), init:stop(), ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Utility

split_json_array([Json, Rest]) ->
    {Json, Rest};

split_json_array(Json) ->
    {Json, nil}.

reply_with_stdfields(Reply, State) ->
    Answer = Reply#{
        <<"allowremote">> => State#state.allowremote,
        <<"daemonid">> => list_to_binary(integer_to_list(State#state.daemonid)),
        <<"NXT">> => list_to_binary(integer_to_list(State#state.nxtaddr)),
        <<"serviceNXT">> => list_to_binary(integer_to_list(State#state.servicenxtaddr)),
        <<"myid">> => list_to_binary(integer_to_list(State#state.myid))
    },
    JsonStr = jsx:encode(Answer),
    snetreply(binary_to_list(JsonStr), State).

%% process good and bad responses from agent
process_reply({ok, Data, _State}, _Req, nil) -> Data;
process_reply({ok, Data, _State}, _Req, Tag) -> Data#{<<"tag">> => Tag};
process_reply({error, _Reason, _State}, _Req, nil) -> #{<<"result">> => <<"no response">>};
process_reply({error, _Reason, _State}, _Req, Tag) -> #{<<"result">> => <<"no response">>, <<"tag">> => Tag}.

%% agent API

%% private
process_json_req(_Error, <<"registered">>, _Method, _Req, State) ->
    Answer = #{
        <<"result">> => <<"activated">>
    },
    {ok, Answer, State};

process_json_req(Error, _Result, _Method, _Req, State) when Error /= nil ->
    Answer = #{
        <<"result">> => <<"completed">>
    },
    {ok, Answer, State};

process_json_req(_Error, Result, _Method, _Req, State) when Result /= nil ->
    Answer = #{
        <<"result">> => <<"completed">>
    },
    {ok, Answer, State};

process_json_req(_Error, _Result, nil, Req, State) ->
    error_logger:info_msg("request (~p) has not method~n", [Req]),
    {error, nil, State};

process_json_req(_Error, _Result, <<"echo">>, Req, State) ->
    Message = maps:get(<<"echostr">>, Req, <<"">>),
    Answer = #{
        <<"result">> => Message
    },
    {ok, Answer, State};

%% no response from agent
process_json_req(_Error, _Result, _Method, _Req, State) ->
    {error, nil, State}.

agent_name() -> <<"echodemo">>.
agent_methods() -> [<<"echo">>].
agent_auth_methods() -> [<<"echo">>].
agent_pub_methods() -> [<<"echo">>].

%% register from JSON object
%% return {ok, State, DisabledMethodsMask}
agent_register(State, _Args) -> {ok, State, 0}.

%% initialize from JSON object. process JSON, return JSON
agent_process_register(State, _Args) ->
    NewState = State#state{allowremote = 1},
    Answer = #{<<"result">> => <<"echodemo init">>},
    {ok, Answer, NewState}.

agent_idle(State) -> {ok, State}.
agent_shutdown(State, _Reason) -> {ok, State}.

agent_process_req(Json, State) ->
    error_logger:info_msg("processing agent request~n", []),
    Result = maps:get(<<"result">>, Json, nil),
    Error = maps:get(<<"error">>, Json, nil),
    Method = maps:get(<<"method">>, Json, nil),
    process_json_req(Error, Result, Method, Json, State).