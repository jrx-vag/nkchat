%% -------------------------------------------------------------------
%%
%% Copyright (c) 2017 Carlos Gonzalez Florido.  All Rights Reserved.
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

-module(nkchat_mm_proxy_callbacks).

-export([plugin_deps/0, plugin_syntax/0, plugin_listen/2]).
-export([nkchat_mm_proxy_http/4]).
-export([nkchat_mm_proxy_ws_init/2, 
         nkchat_mm_proxy_ws_in/2, nkchat_mm_proxy_ws_out/2, 
         nkchat_mm_proxy_ws_terminate/2, nkchat_mm_proxy_ws_handle_call/3,
         nkchat_mm_proxy_ws_handle_cast/2, nkchat_mm_proxy_ws_handle_info/2]).

-include_lib("nkservice/include/nkservice.hrl").



%% ===================================================================
%% Config callbacks
%% ===================================================================


plugin_deps() ->
    [
    ].


plugin_syntax() ->
    code:ensure_loaded(nkchat_mm_proxy_server_ws),
    code:ensure_loaded(nkchat_mm_proxy_server_http),
    % nkpacket:register_protocol(mm_proxy, nkchat_mm_proxy_server_ws),
    #{
        mm_proxy_listen => fun parse_listen/1
    }.


plugin_listen(#{mm_proxy_listen:={parsed, Webs, _Opts}}, #{id:=SrvId}) ->
    Routes = [{'_', [{"/[...]", nkchat_mm_proxy_server_http, [{srv_id, SrvId}]}]}],
    WebOpts =#{
        class => {nkchat_mm_proxy_http, SrvId},
        http_proto => {dispatch, #{routes => Routes}},
        debug => false
    },
    Wss = lists:map(
        fun({nkpacket_protocol_http, Proto, Ip, Port}) ->
            WsProto = case Proto of http -> ws; https -> wss end,
            {nkchat_mm_proxy_server_ws, WsProto, Ip, Port}
        end,
        Webs),
    WsOpts = #{
        class => {nkchat_mm_proxy_ws, SrvId},
        idle_timeout => 60*60*1000,
        path => <<"/api/v3/users/websocket">>,
        debug => false,
        user => #{proxy_to=>{ws, {104,238,188,71}, 8065}},
        get_headers => [<<"cookie">>]
    },                                  
    [{Webs, WebOpts}, {Wss, WsOpts}];

plugin_listen(_Config, _Service) ->
    [].



%% ===================================================================
%% Types
%% ===================================================================

-type state() :: term().
-type continue() :: continue | {continue, list()}.



%% ===================================================================
%% MM proxy
%% ===================================================================


-type http_method() :: nkchat_mm_proxy_server:method().
-type http_path() :: nkchat_mm_proxy_server:path().
-type http_req() :: nkchat_mm_proxy_server:req().
-type http_reply() :: nkchat_mm_proxy_server:reply().


%% @doc called when a new http request has been received
-spec nkchat_mm_proxy_http(http_method(), http_path(), http_req(), state()) ->
    http_reply().

nkchat_mm_proxy_http(_Method, _Path, _Req, State) ->
    {proxy, State}.



%% @doc Called when a new FS proxy connection arrives
-spec nkchat_mm_proxy_ws_init(nkpacket:nkport(), state()) ->
    {ok, state()}.

nkchat_mm_proxy_ws_init(_NkPort, State) ->
    {ok, State}.


% %% @doc Called to select a FS server
% -spec nkchat_mm_proxy_ws_find_fs(nkmedia_service:id(), state()) ->
%     {ok, [chat_mm_engine:id()], state()}.

% nkchat_mm_proxy_ws_find_fs(SrvId, State) ->
%     List = [Name || {Name, _} <- nkmedia_fs_engine:get_all(SrvId)],
%     {ok, List, State}.


%% @doc Called when a new msg arrives
-spec nkchat_mm_proxy_ws_in(map(), state()) ->
    {ok, map(), state()} | {stop, term(), state()} | continue().

nkchat_mm_proxy_ws_in(Msg, State) ->
    {ok, Msg, State}.


%% @doc Called when a new msg is to be answered
-spec nkchat_mm_proxy_ws_out(map(), state()) ->
    {ok, map(), state()} | {stop, term(), state()} | continue().

nkchat_mm_proxy_ws_out(Msg, State) ->
    {ok, Msg, State}.


%% @doc Called when the connection is stopped
-spec nkchat_mm_proxy_ws_terminate(Reason::term(), state()) ->
    {ok, state()}.

nkchat_mm_proxy_ws_terminate(_Reason, State) ->
    {ok, State}.


%% @doc 
-spec nkchat_mm_proxy_ws_handle_call(Msg::term(), {pid(), term()}, state()) ->
    {ok, state()} | continue().

nkchat_mm_proxy_ws_handle_call(Msg, _From, State) ->
    lager:error("Module ~p received unexpected call: ~p", [?MODULE, Msg]),
    {ok, State}.


%% @doc 
-spec nkchat_mm_proxy_ws_handle_cast(Msg::term(), state()) ->
    {ok, state()}.

nkchat_mm_proxy_ws_handle_cast(Msg, State) ->
    lager:error("Module ~p received unexpected cast: ~p", [?MODULE, Msg]),
    {ok, State}.


%% @doc 
-spec nkchat_mm_proxy_ws_handle_info(Msg::term(), state()) ->
    {ok, State::map()}.

nkchat_mm_proxy_ws_handle_info(Msg, State) ->
    lager:error("Module ~p received unexpected info: ~p", [?MODULE, Msg]),
    {ok, State}.






%% ===================================================================
%% Internal
%% ===================================================================



%% ===================================================================
%% Internal
%% ===================================================================



parse_listen({parsed, Conn, Opts}) ->
    {ok, {parsed, Conn, Opts}};

parse_listen(Url) ->
    Opts = #{valid_schemes=>[http, https], resolve_type=>listen},
    case nkpacket:resolve(Url, Opts) of
        {ok, Conn, ConnOpts} -> {ok, {parsed, Conn, ConnOpts}};
        _ -> error
    end.







% parse_listen(_Key, {parsed, Raw, Opts}, _Ctx) ->
%     {ok, {parsed, Raw, Opts}};

% parse_listen(Key, Url, _Ctx) ->
%     Schemes = case Key of
%         dkv_rpc_listen -> [https];
%         _ -> []
%     end,
%     Opts = #{valid_schemes=>Schemes, resolve_type=>listen},
%     case nkpacket:resolve(Url, Opts) of
%         {ok, [Raw], RawOpts} -> {ok, {parsed, Raw, RawOpts}};
%         Err -> lager:warning("ERR: ~p, ~p, ~p", [Url, Opts, Err])
%     end.
