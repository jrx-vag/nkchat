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

-module(nkchat_api).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([cmd/4]).

-include_lib("nkapi/include/nkapi.hrl").
-include_lib("nkservice/include/nkservice.hrl").


%% ===================================================================
%% Commands
%% ===================================================================


%% @doc
cmd(user, get, Req, State) ->
    #nkapi_req{srv_id = SrvId, data = #{user_id:=UserId}} = Req,
    case nkchat_es:read_user(SrvId, UserId) of
        {ok, User, _Vsn} ->
            {ok, User, State};
        {error, Error} ->
            {error, Error, State}
    end;

cmd(user, create, Req, State) ->
    #nkapi_req{srv_id = SrvId, data = Data} = Req,
    UserId = nklib_util:luid(),
    Obj = Data#{
        user_id => UserId,
        created_time => nklib_util:m_timestamp()
    },
    case nkchat_es:write_user(SrvId, UserId, Obj) of
        {ok, _Vsn} ->
            event(SrvId, user, created, UserId, #{}),
            {ok, #{user_id=>UserId}, State};
        {error, Error} ->
            {error, Error, State}
    end;

cmd(user, delete, Req, State) ->
    #nkapi_req{srv_id = SrvId, data = Data} = Req,
    #{user_id:=UserId} = Data,
    case nkchat_es:delete_user(SrvId, UserId) of
        ok ->
            event(SrvId, user, deleted, UserId, #{}),
            {ok, #{}, State};
        {error, Error} ->
            {error, Error, State}
    end;

cmd(user, search, Req, State) ->
    #nkapi_req{srv_id=SrvId, data=Data} = Req,
    case nkchat_es:list_users(SrvId, Data) of
        {ok, Total, List} ->
            {ok, #{total=>Total, data=>List}, State};
        {error, Error} ->
            {error, Error, State}
    end;

cmd(conversation, get, Req, State) ->
    #nkapi_req{srv_id = SrvId, data = #{conversation_id:=ConvId}} = Req,
    case nkchat_es:read_conversation(SrvId, ConvId) of
        {ok, User, _Vsn} ->
            {ok, User, State};
        {error, Error} ->
            {error, Error, State}
    end;

cmd(conversation, create, Req, State) ->
    #nkapi_req{srv_id = SrvId, data = Data} = Req,
    ConvId = nklib_util:luid(),
    Obj = Data#{
        conversation_id => ConvId,
        created_time => nklib_util:m_timestamp()
    },
    case nkchat_es:write_conversation(SrvId, ConvId, Obj) of
        {ok, _Vsn} ->
            event(SrvId, conversation, created, ConvId, #{}),
            {ok, #{conversation_id=>ConvId}, State};
        {error, Error} ->
            {error, Error, State}
    end;

cmd(conversation, search, Req, State) ->
    #nkapi_req{srv_id=SrvId, data=Data} = Req,
    case nkchat_es:list_conversations(SrvId, Data) of
        {ok, Total, List} ->
            {ok, #{total=>Total, data=>List}, State};
        {error, Error} ->
            {error, Error, State}
    end;

cmd(conversation, delete, Req, State) ->
    #nkapi_req{srv_id = SrvId, data = Data} = Req,
    #{conversation_id := ConvId} = Data,
    case nkchat_es:delete_conversation(SrvId, ConvId) of
        ok ->
            event(SrvId, conversation, deleted, ConvId, #{}),
            {ok, #{}, State};
        {error, Error} ->
            {error, Error, State}
    end;

cmd(conversation, add_members, Req, State) ->
    #nkapi_req{srv_id = SrvId, data = Data} = Req,
    #{conversation_id := ConvId, user_ids := UserIds} = Data,
    case get_conv_users(SrvId, ConvId) of
        {ok, OldUserIds, Obj} ->
            UserIds2 = lists:usort(OldUserIds ++ UserIds),
            Obj2 = Obj#{<<"user_ids">>=>UserIds2},
            case nkchat_es:write_conversation(SrvId, ConvId, Obj2) of
                {ok, _Vsn2} ->
                    event(SrvId, conversation, added_members, ConvId,
                          #{user_ids=>UserIds}),
                    {ok, #{}, State};
                {error, Error} ->
                    {error, Error, State}
            end;
        {error, Error} ->
            {error, Error, State}
    end;

cmd(conversation, remove_members, Req, State) ->
    #nkapi_req{srv_id = SrvId, data = Data} = Req,
    #{conversation_id := ConvId, user_ids := UserIds} = Data,
    case get_conv_users(SrvId, ConvId) of
        {ok, OldUserIds, Obj} ->
            UserIds2 = lists:usort(OldUserIds -- UserIds),
            Obj2 = Obj#{<<"user_ids">>:=UserIds2},
            case nkchat_es:write_conversation(SrvId, ConvId, Obj2) of
                {ok, _Vsn} ->
                    event(SrvId, conversation, removed_members, ConvId,
                    #{user_ids=>UserIds}),
                    {ok, #{}, State};
                {error, Error} ->
                    {error, Error, State}
            end;
        {error, Error} ->
            {error, Error, State}
    end;

cmd(conversation, get_members, Req, State) ->
    #nkapi_req{srv_id = SrvId, data = Data} = Req,
    #{conversation_id := ConvId} = Data,
    case get_conv_users(SrvId, ConvId) of
        {ok, Users, _} ->
            {ok, #{user_ids=>Users}, State};
        {error, Error} ->
            {error, Error, State}
    end;

cmd(message, get, Req, State) ->
    #nkapi_req{srv_id = SrvId, data = #{message_id:=MsgId}} = Req,
    case nkchat_es:read_message(SrvId, MsgId) of
        {ok, User, _Vsn} ->
            {ok, User, State};
        {error, Error} ->
            {error, Error, State}
    end;

cmd(message, create, Req, State) ->
    #nkapi_req{srv_id = SrvId, data = Data} = Req,
    MsgId = nklib_util:luid(),
    Obj = Data#{
        message_id => MsgId,
        created_time => nklib_util:m_timestamp()
    },
    case nkchat_es:write_message(SrvId, MsgId, Obj) of
        {ok, _Vsn} ->
            #{conversation_id:=ConvId} = Data,
            event(SrvId, conversation, created_message, ConvId,
                  #{message_id=>MsgId}),
            {ok, #{message_id=>MsgId}, State};
        {error, Error} ->
            {error, Error, State}
    end;

cmd(message, search, Req, State) ->
    #nkapi_req{srv_id=SrvId, data=Data} = Req,
    case nkchat_es:list_messages(SrvId, Data) of
        {ok, Total, List} ->
            {ok, #{total=>Total, data=>List}, State};
        {error, Error} ->
            {error, Error, State}
    end;

cmd(message, update, Req, State) ->
    #nkapi_req{srv_id = SrvId, data = Data} = Req,
    #{message_id:=MsgId, message:=Msg} = Data,
    case nkchat_es:read_message(SrvId, MsgId) of
        {ok, Obj, _Vsn} ->
            Obj2 = Obj#{<<"message">>=>Msg},
            case nkchat_es:write_message(SrvId, MsgId, Obj2) of
                {ok, _Vsn2} ->
                    #{conversation_id:=ConvId} = Data,
                    event(SrvId, conversation, updated_message, ConvId,
                        #{message_id=>MsgId}),
                    {ok, #{}, State};
                {error, Error} ->
                    {error, Error, State}
            end;
        {error, Error} ->
            {error, Error, State}
    end;

cmd(message, delete, Req, State) ->
    #nkapi_req{srv_id = SrvId, data = Data} = Req,
    #{message_id := MsgId} = Data,
    case nkchat_es:delete_message(SrvId, MsgId) of
        ok ->
            #{conversation_id:=ConvId} = Data,
            event(SrvId, conversation, deleted_message, ConvId,
                #{message_id=>MsgId}),
            {ok, #{}, State};
        {error, Error} ->
            {error, Error, State}
    end;

cmd(_Sub, _Cmd, _Req, _State) ->
    continue.


%% ===================================================================
%% Commands
%% ===================================================================

%% @private
get_conv_users(SrvId, ConvId) ->
    case nkchat_es:read_conversation(SrvId, ConvId) of
        {ok, #{<<"user_ids">>:=UserIds} = Obj, _Vsn} ->
            case UserIds of
                <<>> -> {ok, [], Obj};
                _ -> {ok, UserIds, Obj}
            end;
        {error, Error} ->
            {error, Error}
    end.


%% @private
event(SrvId, Sub, Type, ObjId, Body) ->
    Event = #event{
        srv_id = SrvId,
        class = <<"chat">>,
        subclass = nklib_util:to_binary(Sub),
        type = nklib_util:to_binary(Type),
        obj_id = ObjId,
        body = Body
    },
    nkservice_events:send(Event).