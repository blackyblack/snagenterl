-module(agent).

-include("agentinfo.hrl").

-export([agent_name/0, agent_methods/0, agent_auth_methods/0, agent_pub_methods/0,
         agent_register/2, agent_process_register/2, agent_idle/1, agent_shutdown/2,
         agent_process_req/2]).


agent_name() -> <<"echodemo">>.
agent_methods() -> [<<"echo">>].
agent_auth_methods() -> [<<"echo">>].
agent_pub_methods() -> [<<"echo">>].

%% register from JSON object
%% return {ok, State, DisabledMethodsMask}
agent_register(State, _Args) -> {ok, State, 0}.

%% initialize from JSON object. process JSON, return JSON
agent_process_register(State, _Args) ->
    error_logger:info_msg("Hello from echodemo.~n", []),
    NewState = State#agentinfo{allowremote = 1},
    Answer = #{<<"result">> => <<"echodemo init">>},
    {ok, Answer, NewState}.

agent_idle(State) -> {ok, State}.
agent_shutdown(State, _Reason) -> {ok, State}.

agent_process_req(Json, State) ->
    Result = maps:get(<<"result">>, Json, nil),
    Error = maps:get(<<"error">>, Json, nil),
    Method = maps:get(<<"method">>, Json, nil),
    process_json_req(Error, Result, Method, Json, State).

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