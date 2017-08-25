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
-module(nkchat_session_obj_syntax).
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

api(<<"get_conversations">>, Syntax) ->
    Syntax#{
        id => binary
    };

api(<<"get_conversation_info">>, Syntax) ->
    Syntax#{
        id => binary,
        conversation_id => binary,
        '__mandatory' => [conversation_id]
    };

api(<<"set_active_conversation">>, Syntax) ->
    Syntax#{
        id => binary,
        conversation_id => binary,
        '__mandatory' => [conversation_id]
    };

api(<<"add_conversation">>, Syntax) ->
    Syntax#{
        id => binary,
        conversation_id => binary,
        '__mandatory' => [conversation_id]
    };

api(<<"remove_conversation">>, Syntax) ->
    Syntax#{
        id => binary,
        conversation_id => binary,
        '__mandatory' => [conversation_id]
    };

api(<<"send_invitation">>, Syntax) ->
    Syntax#{
        id => binary,
        member_id => binary,
        conversation_id => binary,
        ttl => {integer, 0, none},
        '__mandatory' => [member_id, conversation_id]
    };

api(<<"accept_invitation">>, Syntax) ->
    Syntax#{
        id => binary,
        token =>  binary,
        '__mandatory' => [token]
    };

api(<<"reject_invitation">>, Syntax) ->
    Syntax#{
        id => binary,
        token =>  binary,
        '__mandatory' => [token]
    };

api(<<"launch_notifications">>, Syntax) ->
    Syntax#{
        id => binary
    };

api(<<"wakeup">>, Syntax) ->
    Syntax#{
        id => binary
    };

api(Cmd, Syntax) ->
    nkdomain_obj_syntax:syntax(Cmd, ?CHAT_SESSION, Syntax).
