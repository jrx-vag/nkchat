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
event({message_created, #{obj_id:=MsgId}}, Session) ->
    {event, message_created, #{message_id=>MsgId}, Session};

event({message_updated, #{obj_id:=MsgId}}, Session) ->
    {event, message_updated, #{message_id=>MsgId}, Session};

event({message_deleted, MsgId}, Session) ->
    {event, message_deleted, #{message_id=>MsgId}, Session};

event({member_added, MemberId}, Session) ->
    {event, member_added, #{member_id=>MemberId}, Session};

event({added_to_conversation, MemberId}, #?NKOBJ{obj_id=ConvId}=Session) ->
    {event, added_to_conversation, MemberId, #{conversation_id=>ConvId}, Session};

event({member_removed, MemberId}, Session) ->
    {event, member_removed, #{member_id=>MemberId}, Session};

event({removed_from_conversation, MemberId}, #?NKOBJ{obj_id=ConvId}=Session) ->
    {event, removed_from_conversation, MemberId, #{conversation_id=>ConvId}, Session};

event({session_added, UserId, SessId}, Session) ->
    {event, session_added, #{member_id=>UserId, session_id=>SessId}, Session};

event({session_removed, UserId, SessId}, Session) ->
    {event, session_removed, #{member_id=>UserId, session_id=>SessId}, Session};

event(_Event, Session) ->
    {ok, Session}.





