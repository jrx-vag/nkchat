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
event({conversation_added, ConvId}, Session) ->
    {event, conversation_added, #{conversation_id=>ConvId}, Session};

event({conversation_removed, ConvId}, Session) ->
    {event, conversation_removed, #{conversation_id=>ConvId}, Session};

event({conversation_activated, ConvId}, Session) ->
    {event, conversation_activated, #{conversation_id=>ConvId}, Session};

event({member_added, ConvId, true, MemberId}, Session) ->
    {event, member_added, #{conversation_id=>ConvId, member_id=>MemberId}, Session};

event({member_removed, ConvId, true, MemberId}, Session) ->
    {event, member_removed, #{conversation_id=>ConvId, member_id=>MemberId}, Session};

event({message_created, Msg}, Session) ->
    {event, message_created, #{message=>Msg}, Session};

event({message_updated, Msg}, Session) ->
    {event, message_updated, #{message=>Msg}, Session};

event({message_deleted, MsgId}, Session) ->
    {event, message_deleted, #{message_id=>MsgId}, Session};

event({unread_counter_updated, ConvId, Counter}, Session) ->
    {event, unread_counter_updated, #{conversation_id=>ConvId, counter=>Counter}, Session};

event(_Event, Session) ->
    {ok, Session}.





