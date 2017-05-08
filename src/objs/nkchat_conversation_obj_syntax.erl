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

-export([api/3]).



%% ===================================================================
%% Syntax
%% ===================================================================


%% @doc
api('', create, Syntax) ->
    Syntax#{
        name => binary,
        description => binary,
        subtype => {atom, [private, channel, one2one]},
        domain => binary,
        '__mandatory' => [name, description]
    };

api('', update, Syntax) ->
    Syntax#{
        id => binary,
        description => binary,
        '__mandatory' => [id, description]
    };

api('', add_member, Syntax) ->
    Syntax#{
        id => binary,
        member_id => binary,
        '__mandatory' => [member_id]
    };

api('', remove_member, Syntax) ->
    Syntax#{
        id => binary,
        member_id => binary,
        '__mandatory' => [member_id]
    };

api('', get_member_conversations, Syntax) ->
    Syntax#{
        member_id => binary
    };

api('', get_messages, Syntax) ->
    Syntax#{
        id => binary,
        size => integer,
        from => integer,
        start_date => integer
    };

api(Sub, Cmd, Syntax) ->
    nkdomain_obj_syntax:syntax(Sub, Cmd, Syntax).
