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

-module(nkchat_session_obj_events).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-export([event/2]).

-include_lib("nkdomain/include/nkdomain.hrl").


%% ===================================================================
%% Public
%% ===================================================================


%% @private
event({conversation_added, ConvId}, State) ->
    {event, {conversation_added, #{conversation_id=>ConvId}}, State};

event({conversation_removed, ConvId}, State) ->
    {event, {conversation_removed, #{conversation_id=>ConvId}}, State};

event({conversation_updated, ConvId, Conv}, State) ->
    Conv2 = Conv#{conversation_id => ConvId},
    {event, {conversation_updated, Conv2}, State};

event({invite_added, ConvId, InviteData}, State) ->
    {event, {invite_added, InviteData#{conversation_id=>ConvId}}, State};

event({invite_removed, ConvId, UserId}, State) ->
    {event, {invite_removed, #{conversation_id=>ConvId, user_id=>UserId}}, State};

event({invited_to_conversation, TokenId, UserId, ConvId}, State) ->
    Data = #{token_id=>TokenId, user_id=>UserId, conversation_id=>ConvId},
    {event, {invited_to_conversation, Data}, State};

event({is_closed_updated, ConvId, IsClosed}, State) ->
    {event, {is_closed_updated, #{conversation_id=>ConvId, is_closed=>IsClosed}}, State};

event({last_seen_message, ConvId, MemberIds, Time}, State) ->
    {event, {last_seen_message, #{conversation_id=>ConvId, member_ids=>MemberIds, last_seen_message_time=>Time}}, State};

event({member_added, ConvId, MemberId, MemberData}, State) ->
    {event, {member_added, MemberData#{conversation_id=>ConvId, member_id=>MemberId}}, State};

event({member_muted, ConvId, MemberId, Muted}, State) ->
    {event, {member_muted, #{conversation_id=>ConvId, member_id=>MemberId, is_muted=>Muted}}, State};

event({member_removed, ConvId, MemberId}, State) ->
    {event, {member_removed, #{conversation_id=>ConvId, member_id=>MemberId}}, State};

event({member_typing, ConvId, MemberId}, State) ->
    {event, {member_typing, #{conversation_id=>ConvId, member_id=>MemberId}}, State};

event({message_created, ConvId, Msg}, State) ->
    {event, {message_created, #{conversation_id=>ConvId, message=>Msg}}, State};

event({message_updated, ConvId, Msg}, State) ->
    {event, {message_updated, #{conversation_id=>ConvId, message=>Msg}}, State};

event({message_deleted, ConvId, MsgId}, State) ->
    {event, {message_deleted, #{conversation_id=>ConvId, message_id=>MsgId}}, State};

event({remove_notification, TokenId, Reason}, State) ->
    {event, {remove_notification, #{token_id=>TokenId, reason=>Reason}}, State};

event({status_updated, ConvId, Status}, State) ->
    {event, {status_updated, #{conversation_id=>ConvId, status=>Status}}, State};

event({unread_counter_updated, ConvId, Counter}, State) ->
    {event, {unread_counter_updated, #{conversation_id=>ConvId, counter=>Counter}}, State};

event(_Event, State) ->
    {ok, State}.




