
%% -------------------------------------------------------------------
%%
%% Copyright (c) 2016 Carlos Gonzalez Florido.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

%% @doc 
-module(nkchat_mm_proxy_server_ws).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([get_all/0, send/2]).
-export([transports/1, default_port/1]).
-export([conn_init/1, conn_encode/2, conn_parse/3, conn_stop/3,
         conn_handle_call/4, conn_handle_cast/3, conn_handle_info/3]).


% To debug, set debug => [nkchat_mm_proxy_server_ws]
% To debug nkpacket, set debug in listener (nkservice_util:get_api_sockets())

-define(DEBUG(Txt, Args, State),
    case erlang:get(nkchat_mm_proxy_server_ws_debug) of
        true -> ?LLOG(debug, Txt, Args, State);
        _ -> ok
    end).

-define(LLOG(Type, Txt, Args, State),
    lager:Type("MM Server WS (~s) "++Txt, [State#state.remote | Args])).

-define(MSG(Txt, Args, State),
    case erlang:get(nkchat_mm_proxy_server_ws_debug) of
        true -> print(Txt, Args, State);
        _ -> ok
    end).



%% ===================================================================
%% Types
%% ===================================================================

-type user_state() :: nkchat_mm_proxy:state().


%% ===================================================================
%% Public
%% ===================================================================

-spec get_all() ->
    [{Client::pid(), Server::pid()}].

get_all() ->
    nklib_proc:values(?MODULE).


send(Pid, Msg) ->
    gen_server:cast(Pid, {send, Msg}).



%% ===================================================================
%% Protocol callbacks
%% ===================================================================


-record(state, {
    srv_id :: nkservice:id(),
    remote :: binary(),
    proxy :: pid(),
    user_state :: user_state()
}).


%% @private
-spec transports(nklib:scheme()) ->
    [nkpacket:transport()].

transports(_) -> [wss, ws].

-spec default_port(nkpacket:transport()) ->
    inet:port_number() | invalid.

default_port(ws) -> 9443;
default_port(wss) -> 9443.


-spec conn_init(nkpacket:nkport()) ->
    {ok, #state{}}.

conn_init(NkPort) ->
    {ok, #{headers:=Hds}} = nkpacket:get_meta(NkPort),
    Cookie = nklib_util:get_value(<<"cookie">>, Hds),
    {ok, {_, SrvId}, User} = nkpacket:get_user(NkPort),
    #{proxy_to:={Proto, Ip, Port}} = User,
    {ok, Remote} = nkpacket:get_remote_bin(NkPort),
    State = #state{srv_id=SrvId, remote=Remote},
    ?LLOG(notice, "new connection (~p)", [self()], State),
    {ok, State2} = handle(nkchat_mm_proxy_ws_init, [NkPort], State),
    set_log(State2),
    nkservice_util:register_for_changes(SrvId),
    case nkchat_mm_proxy_client_ws:start(SrvId, Proto, Ip, Port, Cookie, self()) of
        {ok, ProxyPid} ->
            ?LLOG(notice, "connected to MM server ~p", [Ip], State),
            monitor(process, ProxyPid),
            nklib_proc:put(?MODULE, ProxyPid),
            {ok, State2#state{proxy=ProxyPid}};
        {error, _Error} ->
            ?LLOG(warning, "not connected to MM server ~p", [_Error], State),
            {error, no_server_available}
    end.


  
%% @private
-spec conn_parse(term()|close, nkpacket:nkport(), #state{}) ->
    {ok, #state{}} | {stop, term(), #state{}}.

conn_parse(close, _NkPort, State) ->
    {ok, State};

conn_parse({text, Data}, _NkPort, #state{proxy=Pid}=State) ->
    Msg = case nklib_json:decode(Data) of
        error ->
            ?LLOG(warning, "JSON decode error: ~p", [Data], State),
            error(json_decode);
        Json ->
            Json
    end,
    ?MSG("received from client\n~s", [Msg], State),
    case handle(nkchat_mm_proxy_ws_in, [Msg], State) of
        {ok, Msg2, State2} ->
            ok = nkchat_mm_proxy_client:send(Pid, Msg2),
            {ok, State2};
        {stop, Reason, State2} ->
            {stop, Reason, State2}
    end.

%% @private
-spec conn_encode(term(), nkpacket:nkport()) ->
    {ok, nkpacket:outcoming()} | continue | {error, term()}.

conn_encode(Msg, _NkPort) when is_map(Msg) ->
    Json = nklib_json:encode(Msg),
    {ok, {text, Json}};

conn_encode(Msg, _NkPort) when is_binary(Msg) ->
    {ok, {text, Msg}}.


%% @doc Called when the connection received an erlang message
-spec conn_handle_call(term(), term(), nkpacket:nkport(), #state{}) ->
    {ok, #state{}} | {stop, Reason::term(), #state{}}.

% conn_handle_call({send_reply, Event}, From, NkPort, State) ->
%     ?LLOG(info, "sending\n~s", [nklib_json:encode_pretty(Event)], State),
%     case handle(nkchat_mm_proxy_ws_out, [Event], State) of
%         {ok, Event2, State2} ->
%             case nkpacket_connection:send(NkPort, Event2) of
%                 ok -> 
%                     gen_server:reply(From, ok),
%                     {ok, State2};
%                 {error, Error} -> 
%                     gen_server:reply(From, error),
%                     ?LLOG(notice, "error sending event: ~p", [Error], State2),
%                     {stop, normal, State2}
%             end;
%         {stop, Reason, State2} ->
%             {stop, Reason, State2}
%     end;

conn_handle_call(Msg, From, _NkPort, State) ->
    handle(nkchat_mm_proxy_ws_handle_call, [Msg, From], State).


-spec conn_handle_cast(term(), nkpacket:nkport(), #state{}) ->
    {ok, #state{}} | {stop, Reason::term(), #state{}}.

conn_handle_cast({send, Msg}, NkPort, State) ->
    ?MSG("received from server:\n~s", [Msg], State),
    send(Msg, NkPort, State);

conn_handle_cast(Msg, _NkPort, State) ->
    handle(nkchat_mm_proxy_ws_handle_cast, [Msg], State).


%% @doc Called when the connection received an erlang message
-spec conn_handle_info(term(), nkpacket:nkport(), #state{}) ->
    {ok, #state{}} | {stop, Reason::term(), #state{}}.

conn_handle_info({nkservice_updated, _SrvId}, _NkPort, State) ->
    {ok, set_log(State)};

conn_handle_info({'DOWN', _Ref, process, Pid, Reason}, _NkPort, 
                 #state{proxy=Pid}=State) ->
    ?LLOG(notice, "stopped because client WS stopped (~p)", [Reason], State),
    {stop, normal, State};

conn_handle_info(Msg, _NkPort, State) ->
    handle(nkchat_mm_proxy_ws_handle_info, [Msg], State).


%% @doc Called when the connection stops
-spec conn_stop(Reason::term(), nkpacket:nkport(), #state{}) ->
    ok.

conn_stop(Reason, _NkPort, State) ->
    catch handle(nkchat_mm_proxy_ws_terminate, [Reason], State).




%% ===================================================================
%% Internal
%% ===================================================================

%% @private
handle(Fun, Args, State) ->
    nklib_gen_server:handle_any(Fun, Args, State, #state.srv_id, #state.user_state).
    

%% @private
set_log(#state{srv_id=SrvId}=State) ->
    Debug = case nkservice_util:get_debug_info(SrvId, ?MODULE) of
        {true, _} -> true;
        _ -> false
    end,
    % ?LLOG(error, "debug: ~p", [Debug], State),
    put(nkchat_mm_proxy_server_ws_debug, Debug),
    State.


%% @private
send(Msg, NkPort, State) ->
    ?MSG("sending ~s", [Msg], State),
    case do_send(Msg, NkPort) of
        ok -> 
            {ok, State};
        error -> 
            ?LLOG(info, "error sending msg", [], State),
            {stop, normal, State}
    end.


%% @private
do_send(Msg, NkPort) ->
    nkpacket_connection:send(NkPort, Msg).



%% @private
print(Txt, [#{}=Map], State) ->
    print(Txt, [nklib_json:encode_pretty(Map)], State);
print(Txt, Args, State) ->
    ?LLOG(notice, Txt, Args, State).

