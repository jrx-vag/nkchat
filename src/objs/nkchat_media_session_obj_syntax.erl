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

%% @doc Session Object Syntax
-module(nkchat_media_session_obj_syntax).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([syntax/2]).

-include("nkchat.hrl").


%% ===================================================================
%% Syntax
%% ===================================================================


%% @doc
syntax(<<"start">>, Syntax) ->
    Syntax#{
        domain_id => binary,
        session_events => {list, binary}
    };

syntax(<<"get_calls">>, Syntax) ->
    Syntax#{
        id => binary
    };

syntax(<<"get_call_info">>, Syntax) ->
    Syntax#{
        id => binary,
        call_id => binary,
        '__mandatory' => [call_id]
    };

syntax(<<"invite">>, Syntax) ->
    Syntax#{
        id => binary,
        user_id => binary,
        call_name => binary,
        sdp => binary,
        ttl => {integer, 1, 5*60},          % Secs
        trickle_ice => boolean,
        audio => boolean,
        video => boolean,
        '__mandatory' => [user_id, sdp]
    };

syntax(<<"cancel_invite">>, Syntax) ->
    Syntax#{
        id => binary,
        invite_id =>  binary,
        '__mandatory' => [invite_id]
    };

syntax(<<"hangup_call">>, Syntax) ->
    Syntax#{
        id => binary,
        call_id => binary,
        '__mandatory' => [call_id]
    };

syntax(<<"accept_invite">>, Syntax) ->
    Syntax#{
        id => binary,
        invite_id =>  binary,
        sdp => binary,
        trickle_ice => boolean,
        audio => boolean,
        video => boolean,
        '__mandatory' => [invite_id, sdp]
    };

syntax(<<"reject_invite">>, Syntax) ->
    Syntax#{
        id => binary,
        invite_id =>  binary,
        '__mandatory' => [invite_id]
    };

syntax(<<"launch_notifications">>, Syntax) ->
    Syntax#{
        id => binary
    };

syntax(Cmd, Syntax) ->
    nkdomain_obj_syntax:syntax(Cmd, ?CHAT_SESSION, Syntax).
