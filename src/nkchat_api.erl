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

-include_lib("nkservice/include/nkservice.hrl").


%% ===================================================================
%% Commands
%% ===================================================================


%% @doc
cmd(user, create, Req, State) ->
    #api_req{srv_id = SrvId, data = Data} = Req,
    UserId = nklib_util:luid(),
    Obj = Data#{
        user_id => UserId,
        created_time => nklib_util:m_timestamp()
    },
    case nkchat_es:write_user(SrvId, UserId, Obj) of
        {ok, _Vsn} ->
            {ok, #{user_id=>UserId}, State};
        {error, Error} ->
            {error, Error, State}
    end;

cmd(user, delete, Req, State) ->
    #api_req{srv_id = SrvId, data = Data} = Req,
    #{user_id:=UserId} = Data,
    case nkchat_es:delete_user(SrvId, UserId) of
        ok ->
            {ok, #{}, State};
        {error, Error} ->
            {error, Error, State}
    end;

cmd(user, search, Req, State) ->
    #api_req{srv_id=SrvId, data=Data} = Req,
    case nkchat_es:list_users(SrvId, Data) of
        {ok, Total, List} ->
            {ok, #{total=>Total, data=>List}, State};
        {error, Error} ->
            {error, Error, State}
    end;

cmd(conversation, create, Req, State) ->
    #api_req{srv_id = SrvId, data = Data} = Req,
    ConvId = nklib_util:luid(),
    Obj = Data#{
        conversation_id => ConvId,
        created_time => nklib_util:m_timestamp()
    },
    case nkchat_es:write_conversation(SrvId, ConvId, Obj) of
        {ok, _Vsn} ->
            {ok, #{conversation_id=>ConvId}, State};
        {error, Error} ->
            {error, Error, State}
    end;

%%cmd(conversation, search, Req, State) ->
%%    #api_req{srv_id=SrvId, data=Data} = Req,
%%    #{
%%        conversation_id := ConvId,
%%        searc_spec := _Spec
%%    } = Data,
%%    {ok, #{}, State};

cmd(conversation, delete, Req, State) ->
    #api_req{srv_id = SrvId, data = Data} = Req,
    #{conversation_id := ConvId} = Data,
    case nkchat_es:delete_conversation(SrvId, ConvId) of
        ok ->
            {ok, #{}, State};
        {error, Error} ->
            {error, Error, State}
    end;

cmd(conversation, add_members, Req, State) ->
    #api_req{srv_id = SrvId, data = Data} = Req,
    #{conversation_id := ConvId, user_ids := UserIds} = Data,
    case get_conv_users(SrvId, ConvId) of
        {ok, OldUserIds, Obj} ->
            UserIds2 = lists:usort(OldUserIds ++ UserIds),
            Obj2 = Obj#{<<"user_ids">>=>UserIds2},
            case nkchat_es:write_conversation(SrvId, ConvId, Obj2) of
                {ok, _Vsn2} ->
                    {ok, #{}, State};
                {error, Error} ->
                    {error, Error, State}
            end;
        {error, Error} ->
            {error, Error, State}
    end;

cmd(conversation, remove_members, Req, State) ->
    #api_req{srv_id = SrvId, data = Data} = Req,
    #{conversation_id := ConvId, user_ids := UserIds} = Data,
    case get_conv_users(SrvId, ConvId) of
        {ok, OldUserIds, Obj} ->
            UserIds2 = lists:usort(OldUserIds -- UserIds),
            Obj2 = Obj#{<<"user_ids">>:=UserIds2},
            case nkchat_es:write_conversation(SrvId, ConvId, Obj2) of
                {ok, _Vsn} ->
                    {ok, #{}, State};
                {error, Error} ->
                    {error, Error, State}
            end;
        {error, Error} ->
            {error, Error, State}
    end;

cmd(conversation, get_members, Req, State) ->
    #api_req{srv_id = SrvId, data = Data} = Req,
    #{conversation_id := ConvId} = Data,
    case get_conv_users(SrvId, ConvId) of
        {ok, Users, _} ->
            {ok, #{user_ids=>Users}, State};
        {error, Error} ->
            {error, Error, State}
    end;

cmd(message, create, Req, State) ->
    #api_req{srv_id = SrvId, data = Data} = Req,
    MsgId = nklib_util:luid(),
    Obj = Data#{
        message_id => MsgId,
        created_time => nklib_util:m_timestamp()
    },
    case nkchat_es:write_message(SrvId, MsgId, Obj) of
        {ok, _Vsn} ->
            {ok, #{message_id=>MsgId}, State};
        {error, Error} ->
            {error, Error, State}
    end;

%%cmd(message, search, Req, State) ->
%%    #api_req{srv_id=SrvId, data=Data} = Req,
%%    #{
%%        user_id := _UserId,
%%        searc_spec := _Spec
%%    } = Data,
%%    {ok, #{}, State};

cmd(message, update, Req, State) ->
    #api_req{srv_id = SrvId, data = Data} = Req,
    #{message_id:=MsgId, message:=Msg} = Data,
    case nkchat_es:read_message(SrvId, MsgId) of
        {ok, Obj, _Vsn} ->
            Obj2 = Obj#{<<"message">>=>Msg},
            case nkchat_es:write_message(SrvId, MsgId, Obj2) of
                {ok, _Vsn2} ->
                    {ok, #{}, State};
                {error, Error} ->
                    {error, Error, State}
            end;
        {error, Error} ->
            {error, Error, State}
    end;

cmd(message, delete, Req, State) ->
    #api_req{srv_id = SrvId, data = Data} = Req,
    #{message_id := MsgId} = Data,
    case nkchat_es:delete_message(SrvId, MsgId) of
        ok ->
            {ok, #{}, State};
        {error, Error} ->
            {error, Error, State}
    end;

cmd(_Sub, _Cmd, _Req, _State) ->
    continue.


%% ===================================================================
%% Commands
%% ===================================================================


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
