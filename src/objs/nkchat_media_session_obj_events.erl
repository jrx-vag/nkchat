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

-module(nkchat_media_session_obj_events).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-export([event/2]).

-include_lib("nkdomain/include/nkdomain.hrl").


%% ===================================================================
%% Public
%% ===================================================================


%% @private
event({invite, InviteId, CallerId, InviteOpts}, State) ->
    Body = #{invite_id=>InviteId, caller_id=>CallerId, call_data=>InviteOpts},
    {event, {invite, Body}, State};

event({invite_removed, InviteId, Reason}, State) ->
    {event, {invite_removed, #{invite_id=>InviteId, reason=>Reason}}, State};

event({invite_accepted, InviteId, CallId, AcceptOpts}, State) ->
    {event, {invite_accepted, #{invite_id=>InviteId, call_id=>CallId, call_data=>AcceptOpts}}, State};

event({member_added, CallId, MemberId, Roles}, State) ->
    {event, {member_added, #{call_id=>CallId, user_id=>MemberId, roles=>Roles}}, State};

event({member_removed, CallId, MemberId, Roles}, State) ->
    {event, {member_removed, #{call_id=>CallId, user_id=>MemberId, roles=>Roles}}, State};

event({member_down, CallId, MemberId, Roles}, State) ->
    {event, {member_down, #{call_id=>CallId, user_id=>MemberId, roles=>Roles}}, State};

event({call_created, InviteId, CallId, _CallOpts}, State) ->
    {event, {call_created, #{invite_id=>InviteId, call_id=>CallId}}, State};

event({call_hangup, CallId, Reason}, State) ->
    {event, {call_hangup, #{call_id=>CallId, reason=>Reason}}, State};

event(_Event, #?STATE{parent_id=ParentId}=State) ->
    lager:warning("NKLOG Media Event (~s) ~p", [ParentId, _Event]),
    {ok, State}.




