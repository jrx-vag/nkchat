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

%% ===================================================================
%% API
%% ===================================================================

%% @doc
cmd('', create, #{obj_name:=Name, description:=Desc}=Data, #{srv_id:=SrvId}=State) ->
    Domain = case Data of
        #{domain:=Domain0} -> Domain0;
        _ -> nkdomain_util:get_service_domain(SrvId)
    end,
    case nkchat_conversation_obj:create(SrvId, Name, Domain, Desc) of
        {ok, ObjId, Path, _Pid} ->
            {ok, #{obj_id=>ObjId, path=>Path}, State};
        {error, Error} ->
            {error, Error, State}
    end;

cmd('', add_members, #{id:=Id, member_ids:=MemberIds}, #{srv_id:=SrvId}=State) ->
    case nkchat_conversation_obj:add_members(SrvId, Id, MemberIds) of
        {ok, Reply} ->
            {ok, Reply, State};
        {error, Error} ->
            {error, Error, State}
    end;

cmd('', remove_members, #{id:=Id, member_ids:=MemberIds}, #{srv_id:=SrvId}=State) ->
    case nkchat_conversation_obj:remove_members(SrvId, Id, MemberIds) of
        {ok, Reply} ->
            {ok, Reply, State};
        {error, Error} ->
            {error, Error, State}
    end;

cmd('', find_referred, #{id:=Id}=Data, #{srv_id:=SrvId}=State) ->
    Search = nkdomain_user_obj:find_referred(SrvId, Id, Data),
    nkdomain_util:api_search(Search, State);

cmd('', Cmd, Data, State) ->
    nkdomain_util:api_cmd_common(?CHAT_CONVERSATION, Cmd, Data, State);

cmd(_Sub, _Cmd, _Data, State) ->
    {error, not_implemented, State}.
