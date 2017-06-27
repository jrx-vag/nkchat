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

%% @doc Conversation Object Syntax
-module(nkchat_conversation_obj_syntax).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([api/2]).

-include("nkchat.hrl").


%% ===================================================================
%% Syntax
%% ===================================================================


%%%% @doc
api(<<"add_member">>, Syntax) ->
    Syntax#{
        id => binary,
        member_id => binary,
        '__mandatory' => [id, member_id]
    };

api(<<"remove_member">>, Syntax) ->
    Syntax#{
        id => binary,
        member_id => binary,
        '__mandatory' => [id, member_id]
    };

api(<<"find_member_conversations">>, Syntax) ->
    Syntax#{
        member_id => binary
    };

api(<<"get_messages">>, Syntax) ->
    Syntax#{
        id => binary,
        size => integer,
        from => integer,
        start_date => integer,
        '__mandatory' => [id]
    };

api(<<"get_last_messages">>, Syntax) ->
    Syntax#{
        id => binary,
        '__mandatory' => [id]
    };

api(Cmd, Syntax) ->
    nkdomain_obj_syntax:syntax(Cmd, ?CHAT_CONVERSATION, Syntax).
