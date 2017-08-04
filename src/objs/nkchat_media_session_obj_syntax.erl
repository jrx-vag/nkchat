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

-export([api/2]).

-include("nkchat.hrl").


%% ===================================================================
%% Syntax
%% ===================================================================


%% @doc
api(<<"start">>, Syntax) ->
    Syntax#{
        domain_id => binary,
        session_events => {list, binary}
    };

api(<<"get_calls">>, Syntax) ->
    Syntax#{
        id => binary
    };

api(<<"get_call_info">>, Syntax) ->
    Syntax#{
        id => binary,
        call_id => binary,
        '__mandatory' => [call_id]
    };

api(<<"make_call">>, Syntax) ->
    Syntax#{
        id => binary,
        user_id => binary,
        sdp => binary,
        trickle_ice => boolean,
        '__mandatory' => [user_id, sdp]
    };

api(<<"hangup_call">>, Syntax) ->
    Syntax#{
        id => binary,
        call_id => binary,
        '__mandatory' => [call_id]
    };

api(<<"accept_call">>, Syntax) ->
    Syntax#{
        id => binary,
        call_id =>  binary,
        sdp => binary,
        trickle_ice => boolean,
        '__mandatory' => [call_id, sdp]
    };

api(<<"reject_call">>, Syntax) ->
    Syntax#{
        id => binary,
        call_id =>  binary,
        '__mandatory' => [call_id]
    };

api(Cmd, Syntax) ->
    nkdomain_obj_syntax:syntax(Cmd, ?CHAT_SESSION, Syntax).
