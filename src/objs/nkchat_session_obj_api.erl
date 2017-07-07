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

%% @doc Session Object API
-module(nkchat_session_obj_api).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([cmd/2]).

-include("nkchat.hrl").
-include_lib("nkdomain/include/nkdomain.hrl").
-include_lib("nkevent/include/nkevent.hrl").
-include_lib("nkservice/include/nkservice.hrl").


-define(SESSION_DEF_EVENT_TYPES, [
    <<"unloaded">>,
    <<"conversation_added">>,
    <<"conversation_removed">>,
    <<"member_added">>,
    <<"member_removed">>,
    <<"message_created">>,
    <<"message_udpdated">>,
    <<"message_deleted">>,
    <<"unread_counter_updated">>
]).


%% ===================================================================
%% API
%% ===================================================================


%% @doc
cmd(<<"start">>, #nkreq{session_module=nkapi_server}=Req) ->
    #nkreq{data=Data, session_pid=Pid, user_id=UserId, srv_id=SrvId} = Req,
    case nkdomain_api_util:get_id(?DOMAIN_DOMAIN, domain_id, Data, Req) of
        {ok, DomainId} ->
            Opts = #{
                nkapi_server_pid =>Pid,
                session_events => maps:get(session_events, Data, []),
                session_link => {nkapi_server, Pid}
            },
            case nkchat_session_obj:start(SrvId, DomainId, UserId, Opts) of
                {ok, SessId, _Pid} ->
                    Req2 = nkdomain_api_util:add_id(?CHAT_SESSION, SessId, Req),
                    {ok, #{obj_id=>SessId}, Req2};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end;

cmd(<<"stop">>, #nkreq{data=Data, srv_id=SrvId, user_state=UserState}=Req) ->
    case nkdomain_api_util:get_id(?CHAT_SESSION, Data, Req) of
        {ok, SessId} ->
            case nkdomain:unload(SrvId, SessId, user_stop) of
                ok ->
                    {ok, #{}, UserState};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end;

cmd(<<"get_conversations">>, #nkreq{data=Data, srv_id=SrvId}=Req) ->
    case nkdomain_api_util:get_id(?CHAT_SESSION, Data, Req) of
        {ok, Id} ->
            case nkchat_session_obj:get_conversations(SrvId, Id) of
                {ok, List} ->
                    {ok, #{conversation_ids=>List}};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end;

cmd(<<"get_conversation">>, #nkreq{data=#{conversation_id:=ConvId}=Data, srv_id=SrvId}=Req) ->
    case nkdomain_api_util:get_id(?CHAT_SESSION, Data, Req) of
        {ok, Id} ->
            case nkchat_session_obj:get_conversation(SrvId, Id, ConvId) of
                {ok, Data2} ->
                    {ok, Data2};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end;

cmd(<<"set_active_conversation">>, #nkreq{data=#{conversation_id:=ConvId}=Data, srv_id=SrvId}=Req) ->
    case nkdomain_api_util:get_id(?CHAT_SESSION, Data, Req) of
        {ok, Id} ->
            case nkchat_session_obj:set_active_conversation(SrvId, Id, ConvId) of
                ok ->
                    {ok, #{}};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end;

cmd(<<"add_conversation">>, #nkreq{data=#{conversation_id:=ConvId}=Data, srv_id=SrvId}=Req) ->
    case nkdomain_api_util:get_id(?CHAT_SESSION, Data, Req) of
        {ok, Id} ->
            case nkchat_session_obj:add_conversation(SrvId, Id, ConvId) of
                {ok, ObjId} ->
                    {ok, #{conversation_id=>ObjId}};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end;

cmd(<<"remove_conversation">>, #nkreq{data=#{conversation_id:=ConvId}=Data, srv_id=SrvId}=Req) ->
    case nkdomain_api_util:get_id(?CHAT_SESSION, Data, Req) of
        {ok, Id} ->
            case nkchat_session_obj:remove_conversation(SrvId, Id, ConvId) of
                ok ->
                    {ok, #{}};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end;

cmd(Cmd, Req) ->
    nkdomain_obj_api:api(Cmd, ?CHAT_SESSION, Req).



%% ===================================================================
%% Internal
%% ===================================================================
