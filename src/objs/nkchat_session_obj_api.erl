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

-export([cmd/4]).

-include("nkchat.hrl").
-include_lib("nkdomain/include/nkdomain.hrl").
-include_lib("nkevent/include/nkevent.hrl").

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
cmd('', find, Data, #{srv_id:=SrvId}=State) ->
    case get_user_id(Data, State) of
        {ok, UserId} ->
            case nkchat_session_obj:find(SrvId, UserId) of
                {ok, List} ->
                    {ok, #{sessions=>List}, State};
                {error, Error} ->
                    {error, Error, State}
            end;
        Error ->
            Error
    end;

cmd('', create, Data, #{srv_id:=SrvId}=State) ->
    case get_user_id(Data, State) of
        {ok, UserId} ->
            case nkchat_session_obj:create(SrvId, UserId) of
                {ok, ObjId, _Path, _Pid} ->
                    cmd('', start, Data#{id=>ObjId}, State);
                {error, Error} ->
                    {error, Error, State}
            end;
        Error ->
            Error
    end;

cmd('', start, #{id:=Id}=Data, #{srv_id:=SrvId}=State) ->
    case nkchat_session_obj:start(SrvId, Id, self()) of
        {ok, ObjId, Reply} ->
            State2 = nkdomain_api_util:add_id(?CHAT_SESSION, ObjId, State),
            Types = maps:get(events, Data, ?SESSION_DEF_EVENT_TYPES),
            Subs = #{
                srv_id => SrvId,
                class => ?DOMAIN_EVENT_CLASS,
                subclass => ?CHAT_SESSION,
                type => Types,
                obj_id => ObjId
            },
            ok = nkapi_server:subscribe(self(), Subs),
            {ok, Reply#{obj_id=>ObjId}, State2#{nkchat_session_types=>Types}};
        {error, Error} ->
            {error, Error, State}
    end;

cmd('', stop, Data, #{srv_id:=SrvId}=State) ->
    case nkdomain_api_util:get_id(?CHAT_SESSION, Data, State) of
        {ok, Id} ->
            State2 = case State of
                #{nkchat_session_types:=Types} ->
                    Subs = #{
                        srv_id => SrvId,
                        class => ?DOMAIN_EVENT_CLASS,
                        subclass => ?CHAT_SESSION,
                        type => Types,
                        obj_id => Id
                    },
                    nkapi_server:unsubscribe(self(), Subs),
                    maps:remove(nkchat_session_types, State);
                _ ->
                    State
            end,
            case nkchat_session_obj:stop(SrvId, Id) of
                ok ->
                    {ok, #{}, State2};
                {error, Error} ->
                    {error, Error, State2}
            end;
        Error ->
            Error
    end;

cmd('', get_all_conversations, Data, #{srv_id:=SrvId}=State) ->
    case nkdomain_api_util:get_id(?CHAT_SESSION, Data, State) of
        {ok, Id} ->
            case nkchat_session_obj:get_all_conversations(SrvId, Id) of
                {ok, ObjId, Data2} ->
                    {ok, Data2#{obj_id=>ObjId}, State};
                {error, Error} ->
                    {error, Error, State}
            end;
        Error ->
            Error
    end;

cmd('', get_conversation, #{conversation_id:=ConvId}=Data, #{srv_id:=SrvId}=State) ->
    case nkdomain_api_util:get_id(?CHAT_SESSION, Data, State) of
        {ok, Id} ->
            case nkchat_session_obj:get_conversation(SrvId, Id, ConvId) of
                {ok, Reply} ->
                    {ok, Reply, State};
                {error, Error} ->
                    {error, Error, State}
            end;
        Error ->
            Error
    end;

cmd('', set_active_conversation, #{conversation_id:=ConvId}=Data, #{srv_id:=SrvId}=State) ->
    case nkdomain_api_util:get_id(?CHAT_SESSION, Data, State) of
        {ok, Id} ->
            case nkchat_session_obj:set_active_conversation(SrvId, Id, ConvId) of
                {ok, Reply} ->
                    {ok, Reply, State};
                {error, Error} ->
                    {error, Error, State}
            end;
        Error ->
            Error
    end;

cmd('', add_conversation, #{conversation_id:=ConvId}=Data, #{srv_id:=SrvId}=State) ->
    case nkdomain_api_util:get_id(?CHAT_SESSION, Data, State) of
        {ok, Id} ->
            case nkchat_session_obj:add_conversation(SrvId, Id, ConvId) of
                {ok, ObjId} ->
                    {ok, #{conversation_id=>ObjId}, State};
                {error, Error} ->
                    {error, Error, State}
            end;
        Error ->
            Error
    end;

cmd('', remove_conversation, #{conversation_id:=ConvId}=Data, #{srv_id:=SrvId}=State) ->
    case nkdomain_api_util:get_id(?CHAT_SESSION, Data, State) of
        {ok, Id} ->
            case nkchat_session_obj:remove_conversation(SrvId, Id, ConvId) of
                ok ->
                    {ok, #{}, State};
                {error, Error} ->
                    {error, Error, State}
            end;
        Error ->
            Error
    end;

cmd(Sub, Cmd, Data, State) ->
    nkdomain_obj_api:api(Sub, Cmd, Data, ?CHAT_SESSION, State).



%% ===================================================================
%% Internal
%% ===================================================================

%% @private
get_user_id(#{user_id:=UserId}, _State) ->
    {ok, UserId};
get_user_id(_, #{user_id:=UserId}) when UserId /= <<>> ->
    {ok, UserId};
get_user_id(_Data, State) ->
    {error, missing_user_id, State}.
