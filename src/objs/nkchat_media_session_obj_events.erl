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
%%event({invite, TokenId, CallerId, CallData}, State) ->
%%    Body = #{invite_id=>TokenId, caller_id=>CallerId, call_data=>CallData},
%%    {event, {invite, Body}, State};
%%
%%event({invite_cancelled, TokenId, Reason}, State) ->
%%    {event, {invite_cancelled, #{invite_id=>TokenId, reason=>Reason}}, State};

event(_Event, #?STATE{parent_id=ParentId}=State) ->
    lager:error("NKLOG MS Event (~s) ~p", [ParentId, _Event]),
    {ok, State}.




