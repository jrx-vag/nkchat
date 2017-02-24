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

-export([plugin_deps/0, error_code/1]).
-export([api_server_cmd/2, api_server_syntax/4]).
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




error_code(obj_not_found)   		-> {0, "Object not found"};
error_code(_) -> continue.




%% ===================================================================
%% API CMD
%% ===================================================================

%% @private
api_server_cmd(
    #api_req{class=chat, subclass=Sub, cmd=Cmd}=Req, State) ->
    nkchat_api:cmd(Sub, Cmd, Req, State);

api_server_cmd(_Req, _State) ->
    continue.


%% @private
api_server_syntax(#api_req{class=chat, subclass=Sub, cmd=Cmd},
    Syntax, Defaults, Mandatory) ->
    nkchat_syntax:syntax(Sub, Cmd, Syntax, Defaults, Mandatory);

api_server_syntax(_Req, _Syntax, _Defaults, _Mandatory) ->
    continue.






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

