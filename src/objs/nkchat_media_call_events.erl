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

-module(nkchat_media_call_events).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([event/2]).

-include("nkchat.hrl").
-include_lib("nkdomain/include/nkdomain.hrl").

%% ===================================================================
%% Types
%% ===================================================================


%% ===================================================================
%% Public
%% ===================================================================


%% @private
event({member_added, MemberId, Roles, SessId, _Pid}, State) ->
    {event, {member_added, #{member_id=>MemberId, roles=>Roles, session_id=>SessId}}, State};

event({member_removed, MemberId, Roles, SessId}, State) ->
    {event, {member_removed, #{member_id=>MemberId, roles=>Roles, session_id=>SessId}}, State};

event({member_down, MemberId, Roles}, State) ->
    {event, {member_down, #{member_id=>MemberId, roles=>Roles}}, State};

event({new_candidate, #sdp_candidate{mid=MId, index=Index, candidate=Candidate}}, State) ->
    {event, {new_candidate, #{sdp_mid=>MId, sdp_line_index=>Index, sdp_candidate=>Candidate}}, State};

event({member_status, MemberId, Status}, State) ->
    {event, {member_status, #{member_id=>MemberId, status=>Status}}, State};

event(_Event, State) ->
    {ok, State}.





