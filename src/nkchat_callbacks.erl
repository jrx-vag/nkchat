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

-module(nkchat_callbacks).

-export([plugin_deps/0, plugin_listen/2]).
-export([chat_mm_proxy_init/2, 
         chat_mm_proxy_in/2, chat_mm_proxy_out/2, 
         chat_mm_proxy_terminate/2, chat_mm_proxy_handle_call/3,
         chat_mm_proxy_handle_cast/2, chat_mm_proxy_handle_info/2]).

-include_lib("nkservice/include/nkservice.hrl").



%% ===================================================================
%% Config callbacks
%% ===================================================================


plugin_deps() ->
    [
    ].


plugin_listen(_Config, #{id:=SrvId}) ->
    code:ensure_loaded(chat_mm_proxy_server),
    Web = {nkpacket_protocol_http, http, {0,0,0,0}, 9443},
    WebOpts =#{
        class => {nkchat_mm, SrvId},
        http_proto => {dispatch, #{routes => [{'_', [{"/[...]", nkchat_mm_proxy_web, []}]}]}}
    },
    Ws = {chat_mm_proxy_server, ws, {0,0,0,0}, 9443},
    WsOpts = #{
        class => {chat_mm_ws, SrvId},
        idle_timeout => 60*60*1000,
        path => <<"/api/v3/users/websocket">>
    },                                  
    [{[Web], WebOpts}, {[Ws], WsOpts}].




%% ===================================================================
%% Types
%% ===================================================================

-type state() :: term().
-type continue() :: continue | {continue, list()}.



%% ===================================================================
%% MM proxy
%% ===================================================================



%% @doc Called when a new FS proxy connection arrives
-spec chat_mm_proxy_init(nkpacket:nkport(), state()) ->
    {ok, state()}.

chat_mm_proxy_init(_NkPort, State) ->
    {ok, State}.


% %% @doc Called to select a FS server
% -spec chat_mm_proxy_find_fs(nkmedia_service:id(), state()) ->
%     {ok, [chat_mm_engine:id()], state()}.

% chat_mm_proxy_find_fs(SrvId, State) ->
%     List = [Name || {Name, _} <- nkmedia_fs_engine:get_all(SrvId)],
%     {ok, List, State}.


%% @doc Called when a new msg arrives
-spec chat_mm_proxy_in(map(), state()) ->
    {ok, map(), state()} | {stop, term(), state()} | continue().

chat_mm_proxy_in(Msg, State) ->
    {ok, Msg, State}.


%% @doc Called when a new msg is to be answered
-spec chat_mm_proxy_out(map(), state()) ->
    {ok, map(), state()} | {stop, term(), state()} | continue().

chat_mm_proxy_out(Msg, State) ->
    {ok, Msg, State}.


%% @doc Called when the connection is stopped
-spec chat_mm_proxy_terminate(Reason::term(), state()) ->
    {ok, state()}.

chat_mm_proxy_terminate(_Reason, State) ->
    {ok, State}.


%% @doc 
-spec chat_mm_proxy_handle_call(Msg::term(), {pid(), term()}, state()) ->
    {ok, state()} | continue().

chat_mm_proxy_handle_call(Msg, _From, State) ->
    lager:error("Module ~p received unexpected call: ~p", [?MODULE, Msg]),
    {ok, State}.


%% @doc 
-spec chat_mm_proxy_handle_cast(Msg::term(), state()) ->
    {ok, state()}.

chat_mm_proxy_handle_cast(Msg, State) ->
    lager:error("Module ~p received unexpected cast: ~p", [?MODULE, Msg]),
    {ok, State}.


%% @doc 
-spec chat_mm_proxy_handle_info(Msg::term(), state()) ->
    {ok, State::map()}.

chat_mm_proxy_handle_info(Msg, State) ->
    lager:error("Module ~p received unexpected info: ~p", [?MODULE, Msg]),
    {ok, State}.






%% ===================================================================
%% Internal
%% ===================================================================


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
