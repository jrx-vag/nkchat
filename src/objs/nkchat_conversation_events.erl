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

-module(nkchat_conversation_events).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([event/2]).
-export_type([events/0]).

-include_lib("nkdomain/include/nkdomain.hrl").

%% ===================================================================
%% Types
%% ===================================================================


-type events() ::
    {message_created, nkdomain:obj()} |
    {message_updated, nkdomain:obj()} |
    {message_deleted, nkdomain:obj_id()} |
    {added_member, nkdomain:obj_id()} |
    {removed_member, nkdomain:obj_id()} |
    {added_session, Member::nkdomain:obj_id(), SessId::nkdomain:obj_id()} |
    {removed_session, Member::nkdomain:obj_id(), SessId::nkdomain:obj_id()}.


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

event({added_member, MemberId}, #?STATE{obj_id=ConvId}=Session) ->
    %% TODO: use nkdomain_obj_util:event?
    nkdomain_obj_util:send_event(added_to_conversation, MemberId, #{conversation_id=>ConvId}, Session),
    {event, added_member, #{member_id=>MemberId}, Session};

event({removed_member, MemberId}, #?STATE{obj_id=ConvId}=Session) ->
    nkdomain_obj_util:send_event(removed_from_conversation, MemberId, #{conversation_id=>ConvId}, Session),
    {event, removed_member, #{member_id=>MemberId}, Session};

event({added_session, UserId, SessId}, Session) ->
    {event, added_session, #{member_id=>UserId, session_id=>SessId}, Session};

event({removed_session, UserId, SessId}, Session) ->
    {event, removed_session, #{member_id=>UserId, session_id=>SessId}, Session};

event(_Event, Session) ->
    {ok, Session}.





