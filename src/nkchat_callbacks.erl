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

-module(nkchat_callbacks).

-export([error/1]).


%% ===================================================================
%% Errors
%% ===================================================================

%% @doc
error(call_hangup)                      -> "Call has hangup";
error(call_down)                        -> "Call process down";
error(call_status_invalid)              -> "Invalid call status";
error(caller_cancelled)                 -> "Caller cancelled call";
error(callee_rejected)                  -> "Callee rejected call";
error(media_not_found)                  -> "Media not found";
error(no_remaining_medias)              -> "No remaining media session";
error(invite_not_allowed)               -> "Invite is not allowed now";
error(media_already_answered)           -> "Media session already answered";
error(user_hangup)                      -> "User started hangup";


error(conversation_not_found)           -> "Conversation not found";
error(conversation_is_already_present)  -> "Conversation is already a member";
error(conversation_is_disabled)         -> "Conversation is currently disabled";
error(conversation_is_closed)           -> "Conversation is closed";
error(invite_not_found)                 -> "Invite not found";
error(_)   		                        -> continue.




%% ===================================================================
%% Admin
%% ===================================================================


%%%% @doc
%%admin_element_action(ElementId, Action, Value, Updates, State) ->
%%    nkchat_admin_tree:element_action(ElementId, Action, Value, Updates, State).
%%
%%
%%%% @doc
%%admin_get_data(ElementId, Spec, State) ->
%%    nkchat_admin_detail:get_data(ElementId, Spec, State).


%% ===================================================================
%% API CMD
%% =====================


%% ===================================================================
%% Types
%% ===================================================================

%-type continue() :: continue | {continue, list()}.


%% ===================================================================
%% Service
%% ===================================================================




