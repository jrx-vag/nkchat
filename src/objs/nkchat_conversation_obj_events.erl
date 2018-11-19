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

%% @doc Conversation Object

-module(nkchat_conversation_obj_events).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-export([event/2]).

-include_lib("nkdomain/include/nkdomain.hrl").

%% ===================================================================
%% Public
%% ===================================================================


%% @private
event({added_to_conversation, MemberId}, #obj_state{id=#obj_id_ext{obj_id=ConvId}}=State) ->
    {event, {added_to_conversation, MemberId, #{conversation_id=>ConvId}}, State};

event({invite_added, InviteData}, State) ->
    {event, {invite_added, InviteData}, State};

event({invite_removed, UserId}, State) ->
    {event, {invite_removed, #{user_id=>UserId}}, State};

event({last_seen_message, MemberIds, Time}, State) ->
    {event, {last_seen_message, #{member_ids => MemberIds, last_seen_message_time => Time}}, State};

event({member_added, MemberId, MemberData}, State) ->
    {event, {member_added, MemberData#{member_id=>MemberId}}, State};

event({member_muted, MemberId, Muted}, State) ->
    {event, {member_muted, #{member_id=>MemberId, is_muted=>Muted}}, State};

event({member_removed, MemberId}, State) ->
    {event, {member_removed, #{member_id=>MemberId}}, State};

event({member_typing, MemberId}, State) ->
    {event, {member_typing, #{member_id=>MemberId}}, State};

event({message_created, #{obj_id:=MsgId}}, State) ->
    {event, {message_created, #{message_id=>MsgId}}, State};

event({message_updated, #{obj_id:=MsgId}}, State) ->
    {event, {message_updated, #{message_id=>MsgId}}, State};

event({message_deleted, MsgId}, State) ->
    {event, {message_deleted, #{message_id=>MsgId}}, State};

event({removed_from_conversation, MemberId}, #obj_state{id=#obj_id_ext{obj_id=ConvId}}=State) ->
    {event, {removed_from_conversation, MemberId, #{conversation_id=>ConvId}}, State};

event({session_added, UserId, SessId}, State) ->
    {event, {session_added, #{member_id=>UserId, session_id=>SessId}}, State};

event({session_removed, UserId, SessId}, State) ->
    {event, {session_removed, #{member_id=>UserId, session_id=>SessId}}, State};

event(_Event, State) ->
    {ok, State}.





