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
cmd(<<"find">>, #nkreq{data=Data, srv_id=SrvId}=Req) ->
    case get_user_id(Data, Req) of
        {ok, UserId} ->
            case nkchat_session_obj:find(SrvId, UserId) of
                {ok, List} ->
                    {ok, #{sessions=>List}};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end;

cmd(<<"create">>, #nkreq{data=Data, srv_id=SrvId}=Req) ->
    case get_user_id(Data, Req) of
        {ok, UserId} ->
            case nkchat_session_obj:create(SrvId, UserId) of
                {ok, #{obj_id:=ObjId}, _Pid} ->
                    cmd(<<"start">>, Req#nkreq{data=Data#{id=>ObjId}});
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end;

cmd(<<"start">>, #nkreq{data=#{id:=Id}=Data, srv_id=SrvId}=Req) ->
    case nkchat_session_obj:start(SrvId, Id, self()) of
        {ok, ObjId, Reply} ->
            Types = maps:get(events, Data, ?SESSION_DEF_EVENT_TYPES),
            Subs = #{
                srv_id => SrvId,
                class => ?DOMAIN_EVENT_CLASS,
                subclass => ?CHAT_SESSION,
                type => Types,
                obj_id => ObjId
            },
            ok = nkapi_server:subscribe(self(), Subs),
            UserMeta1 = nkdomain_api_util:add_id(?CHAT_SESSION, ObjId, Req),
            UserMeta2 = UserMeta1#{nkchat_session_types=>Types},
            {ok, Reply#{obj_id=>ObjId}, UserMeta2};
        {error, Error} ->
            {error, Error}
    end;

cmd(<<"start">>, #nkreq{data=Data}=Req) ->
    case cmd(<<"find">>, Req) of
        {ok, #{sessions:=[#{<<"obj_id">>:=SessId}|_]}} ->
            cmd(<<"start">>, Req#nkreq{data=Data#{id=>SessId}});
        {ok, #{sessions:=[]}} ->
            {error, session_not_found};
        {error, Error2} ->
            {error, Error2 }
    end;

cmd(<<"stop">>, #nkreq{data=Data, srv_id=SrvId, user_meta=UserMeta}=Req) ->
    case nkdomain_api_util:get_id(?CHAT_SESSION, Data, Req) of
        {ok, Id} ->
            UserMeta2 = case UserMeta of
                #{nkchat_session_types:=Types} ->
                    Subs = #{
                        srv_id => SrvId,
                        class => ?DOMAIN_EVENT_CLASS,
                        subclass => ?CHAT_SESSION,
                        type => Types,
                        obj_id => Id
                    },
                    nkapi_server:unsubscribe(self(), Subs),
                    maps:remove(nkchat_session_types, UserMeta);
                _ ->
                    UserMeta
            end,
            case nkchat_session_obj:stop(SrvId, Id) of
                ok ->
                    {ok, #{}, UserMeta2};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end;

cmd(<<"get_all_conversations">>, #nkreq{data=Data, srv_id=SrvId}=Req) ->
    case nkdomain_api_util:get_id(?CHAT_SESSION, Data, Req) of
        {ok, Id} ->
            case nkchat_session_obj:get_all_conversations(SrvId, Id) of
                {ok, ObjId, Data2} ->
                    {ok, Data2#{obj_id=>ObjId}};
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
                {ok, Reply} ->
                    {ok, Reply};
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
                {ok, Reply} ->
                    {ok, Reply};
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

%% @private
get_user_id(#{user_id:=UserId}, _Req) ->
    {ok, UserId};
get_user_id(_, #nkreq{user_id=UserId}) when UserId /= <<>> ->
    {ok, UserId};
get_user_id(_Data, _Req) ->
    {error, missing_user_id}.
