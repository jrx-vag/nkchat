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

%% @doc Conversation Object API
-module(nkchat_conversation_obj_api).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([cmd/4]).

-include("nkchat.hrl").
-include_lib("nkdomain/include/nkdomain.hrl").

%% ===================================================================
%% API
%% ===================================================================

cmd('', create, Data, State) ->
    #{obj_name:=Name, description:=Desc} = Data,
    #{srv_id:=SrvId, domain:=Domain} = State,
    case nkchat_conversation_obj:create(SrvId, Domain, Name, Desc) of
        {ok, ObjId, Path, _Pid} ->
            State2 = nkdomain_api_util:add_id(?CHAT_CONVERSATION, ObjId, State),
            {ok, #{obj_id=>ObjId, path=>Path}, State2};
        {error, Error} ->
            {error, Error, State}
    end;

cmd('', add_member, #{member_id:=MemberId}=Data, #{srv_id:=SrvId}=State) ->
    case nkdomain_api_util:getid(?CHAT_CONVERSATION, Data, State) of
        {ok, Id} ->
            case nkchat_conversation_obj:add_member(SrvId, Id, MemberId) of
                {ok, MemberObjId} ->
                    {ok, #{member_obj_id=>MemberObjId}, State};
                {error, Error} ->
                    {error, Error, State}
            end;
        Error ->
            Error
    end;

cmd('', remove_member, #{member_id:=MemberId}=Data, #{srv_id:=SrvId}=State) ->
    case nkdomain_api_util:getid(?CHAT_CONVERSATION, Data, State) of
        {ok, Id} ->
            case nkchat_conversation_obj:remove_member(SrvId, Id, MemberId) of
                ok ->
                    {ok, #{}, State};
                {error, Error} ->
                    {error, Error, State}
            end;
        Error ->
            Error
    end;

cmd('', get_messages, Data, #{srv_id:=SrvId}=State) ->
    case nkdomain_api_util:getid(?CHAT_CONVERSATION, Data, State) of
        {ok, Id} ->
            case nkchat_conversation_obj:get_messages(SrvId, Id, Data) of
                {ok, Reply} ->
                    {ok, Reply, State};
                {error, Error} ->
                    {error, Error, State}
            end;
        Error ->
            Error
    end;

cmd(Sub, Cmd, Data, State) ->
    nkdomain_api_util:cmd_common(Sub, Cmd, Data, ?CHAT_CONVERSATION, State).

