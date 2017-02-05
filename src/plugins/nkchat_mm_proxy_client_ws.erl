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
-module(nkchat_mm_proxy_client_ws).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([transports/1, default_port/1]).
-export([conn_init/1, conn_encode/2, conn_parse/3, conn_stop/3]).
-export([conn_handle_call/4, conn_handle_cast/3, conn_handle_info/3]).
-export([send/2, print/3]).
-export([start/6, get_all/0]).


% To debug, set debug => [?MODULE]

-define(DEBUG(Txt, Args, State),
    case erlang:get(nkchat_mm_proxy_client_ws_debug) of
        true -> ?LLOG(debug, Txt, Args, State);
        _ -> ok
    end).

-define(MSG(Txt, Args, State),
    case erlang:get(nkchat_mm_proxy_client_ws_debug) of
        true -> print(Txt, Args, State);
        _ -> ok
    end).

-define(LLOG(Type, Txt, Args, State),
    lager:Type("MM Client WS (~s) "++Txt, [State#state.remote|Args])).


-define(WS_TIMEOUT, 60*60*1000).


%% ===================================================================
%% Types
%% ===================================================================



%% ===================================================================
%% Public
%% ===================================================================

start(SrvId, Proto, Ip, Port, Cookie, Pid) ->
    % Debug = case nkservice_util:get_debug_info(SrvId, ?MODULE) of
    %     {true, #{nkpacket:=true}} -> true;
    %     _ -> false
    % end,
    ConnOpts = #{
        class => ?MODULE,
        monitor => self(),
        idle_timeout => ?WS_TIMEOUT,
        user => #{srv_id=>SrvId, proxy=>Pid},
        path => <<"/api/v3/users/websocket">>,
        debug => false,
        headers => [{<<"Cookie">>, Cookie}]
    },
    Conn = {?MODULE, Proto, Ip, Port},
    nkpacket:connect(Conn, ConnOpts).


send(Pid, Msg) ->
    gen_server:cast(Pid, {send, Msg}).



% %% @doc 
% stop(SessId) ->
%     cast(SessId, stop).



%% @private
-spec get_all() ->
    [{Server::pid(), Client::pid()}].

get_all() ->
    nklib_proc:values(?MODULE).


% %% @private
% call(SessId, Msg) ->
%     case find(SessId) of
%         {ok, Pid} -> nkservice_util:call(Pid, Msg, ?CALL_TIMEOUT);
%         not_found -> {error, verto_client_not_found}
%     end.


% %% @private
% cast(SessId, Msg) ->
%     case find(SessId) of
%         {ok, Pid} -> gen_server:cast(Pid, Msg);
%         not_found -> {error, verto_client_not_found}
%     end.


% %% @private
% find(Pid) when is_pid(Pid) ->
%     {ok, Pid};

% find(Id) ->
%     case nklib_proc:values({?MODULE, Id}) of
%         [{undefined, Pid}] -> {ok, Pid};
%         [] -> not_found
%     end.


%% ===================================================================
%% Protocol callbacks
%% ===================================================================

-record(state, {
    srv_id :: nkservice:id(),
    proxy :: pid(),
    remote :: binary()
}).


%% @private
-spec transports(nklib:scheme()) ->
    [nkpacket:transport()].

transports(_) -> [wss, ws].

-spec default_port(nkpacket:transport()) ->
    inet:port_number() | invalid.

default_port(ws) -> 8081;
default_port(wss) -> 8082.


-spec conn_init(nkpacket:nkport()) ->
    {ok, #state{}}.

%% TODO: Send and receive pings from session when they are not in same cluster
conn_init(NkPort) ->
    {ok, Remote} = nkpacket:get_remote_bin(NkPort),
    {ok, _Class, User} = nkpacket:get_user(NkPort),
    #{srv_id:=SrvId, proxy:=ProxyPid} = User,
    State = #state{
        srv_id = SrvId,
        proxy = ProxyPid,
        remote = Remote
    },
    set_log(State),
    nkservice_util:register_for_changes(SrvId),
    monitor(process, ProxyPid),
    ?DEBUG("new session (~p)", [self()], State),
    nklib_proc:put(?MODULE, ProxyPid),
    {ok, State}.


%% @private
-spec conn_parse(term()|close, nkpacket:nkport(), #state{}) ->
    {ok, #state{}} | {stop, term(), #state{}}.

conn_parse(close, _NkPort, State) ->
    {ok, State};


conn_parse({text, Data}, _NkPort, State) ->
    Msg = nklib_json:decode(Data),
    case Msg of
        error -> 
            ?LLOG(warning, "JSON decode error: ~p", [Data], State),
            error(json_decode);
        _ ->
            ok
    end,
    ?MSG("received from server\n~s", [Msg], State),
    #state{proxy=ProxyPid} = State,
    nkchat_mm_proxy_server_ws:send(ProxyPid, Msg),
    {ok, State}.


-spec conn_encode(term(), nkpacket:nkport()) ->
    {ok, nkpacket:outcoming()} | continue | {error, term()}.

conn_encode(Msg, _NkPort) when is_map(Msg) ->
    Json = nklib_json:encode(Msg),
    {ok, {text, Json}};

conn_encode(Msg, _NkPort) when is_binary(Msg) ->
    {ok, {text, Msg}}.


%% @private
-spec conn_handle_call(term(), {pid(), term()}, nkpacket:nkport(), #state{}) ->
    {ok, #state{}} | {stop, term(), #state{}}.

conn_handle_call(get_state, From, _NkPort, State) ->
    gen_server:reply(From, State),
    {ok, State};

conn_handle_call(Msg, _From, _NkPort, State) ->
    lager:error("Module ~p received unexpected call: ~p", [?MODULE, Msg]),
    {stop, unexpected_call, State}.


-spec conn_handle_cast(term(), nkpacket:nkport(), #state{}) ->
    {ok, #state{}} | {stop, term(), #state{}}.

conn_handle_cast({send, Msg}, NkPort, State) ->
    ?MSG("received from client:\n~s", [Msg], State),
    send(Msg, NkPort, State);

conn_handle_cast(stop, _NkPort, State) ->
    ?DEBUG("received stop", [], State),
    {stop, normal, State};

conn_handle_cast(Msg, _NkPort, State) ->
    lager:error("Module ~p received unexpected call: ~p", [?MODULE, Msg]),
    {ok, State}.


%% @private
-spec conn_handle_info(term(), nkpacket:nkport(), #state{}) ->
    {ok, #state{}} | {stop, term(), #state{}}.

conn_handle_info({nkservice_updated, _SrvId}, _NkPort, State) ->
    {ok, set_log(State)};

conn_handle_info({'DOWN', _Ref, process, Pid, Reason}, _NkPort, 
                 #state{proxy=Pid}=State) ->
    ?LLOG(notice, "stopped because server WS stopped (~p)", [Reason], State),
    {stop, normal, State};

conn_handle_info(Msg, _NkPort, State) ->
    lager:warning("Module ~p received unexpected info: ~p", [?MODULE, Msg]),
    {ok, State}.


%% @doc Called when the connection stops
-spec conn_stop(Reason::term(), nkpacket:nkport(), #state{}) ->
    ok.

conn_stop(Reason, _NkPort, #state{}=State) ->
    ?LLOG(notice, "connection stop: ~p", [Reason], State).


%% ===================================================================
%% Requests
%% ===================================================================




%% ===================================================================
%% Util
%% ===================================================================

%% @private
set_log(#state{srv_id=SrvId}=State) ->
    Debug = case nkservice_util:get_debug_info(SrvId, ?MODULE) of
        {true, _} -> true;
        _ -> false
    end,
    % ?LLOG(error, "debug: ~p", [Debug], State),
    put(nkchat_mm_proxy_client_ws_debug, Debug),
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
    ?LLOG(warning, Txt, Args, State).








