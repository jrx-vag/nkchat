%% -------------------------------------------------------------------
%%
%% Copyright (c) 2016 Carlos Gonzalez Florido.  All Rights Reserved.
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

-module(nkchat_syntax).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([syntax/5, get_info/1]).


%% ===================================================================
%% Syntax
%% ===================================================================

syntax(user, create, Syntax, Defaults, Mandatory) ->
    {
        Syntax#{
            login => binary,
            name => binary,
            surname => binary,
            password => binary
        },
        Defaults,
        [login, name, password|Mandatory]
    };

syntax(user, delete, Syntax, Defaults, Mandatory) ->
    {
        Syntax#{
            user_id => binary
        },
        Defaults,
        [user_id|Mandatory]
    };

syntax(user, search, Syntax, Defaults, Mandatory) ->
    {
        search_syntax(Syntax),
        Defaults,
        Mandatory
    };

syntax(conversation, create, Syntax, Defaults, Mandatory) ->
    {
        Syntax#{
            name => binary,
            description => binary,
            user_ids => {list, binary}
        },
        Defaults#{user_ids=>[]},
        [name|Mandatory]
    };

syntax(conversation, search, Syntax, Defaults, Mandatory) ->
    {
        search_syntax(Syntax),
        Defaults,
        Mandatory
    };

syntax(conversation, delete, Syntax, Defaults, Mandatory) ->
    {
        Syntax#{
            conversation_id => binary
        },
        Defaults,
        [conversation_id|Mandatory]
    };

syntax(conversation, add_members, Syntax, Defaults, Mandatory) ->
    {
        Syntax#{
            conversation_id => binary,
            user_ids => {list, binary}
        },
        Defaults,
        [conversation_id, user_ids|Mandatory]
    };

syntax(conversation, remove_members, Syntax, Defaults, Mandatory) ->
    {
        Syntax#{
            conversation_id => binary,
            user_ids => {list, binary}
        },
        Defaults,
        [conversation_id, user_ids|Mandatory]
    };

syntax(conversation, get_members, Syntax, Defaults, Mandatory) ->
    {
        Syntax#{
            conversation_id => binary
        },
        Defaults,
        [conversation_id|Mandatory]
    };

syntax(message, create, Syntax, Defaults, Mandatory) ->
    {
        Syntax#{
            conversation_id => binary,
            user_id => binary,
            message => binary
        },
        Defaults,
        [conversation_id, user_id, message|Mandatory]
    };

syntax(message, search, Syntax, Defaults, Mandatory) ->
    {
        search_syntax(Syntax),
        Defaults,
        Mandatory
    };

syntax(message, update, Syntax, Defaults, Mandatory) ->
    {
        Syntax#{
            message_id => binary,
            message => binary
        },
        Defaults,
        [message_id, message|Mandatory]
    };

syntax(message, delete, Syntax, Defaults, Mandatory) ->
    {
        Syntax#{
            message_id => binary
        },
        Defaults,
        [message_id|Mandatory]
    };

syntax(_Sub, _Cmd, Syntax, Defaults, Mandatory) ->
    {Syntax, Defaults, Mandatory}.



%% ===================================================================
%% Internal
%% ===================================================================


search_syntax(Base) ->
    Base#{
        fields => {list, binary},                          %% Special case for [<<"_all">>]
        filter => map,
        size => pos_integer,
        from => pos_integer,
        sort_by => {list, binary},
        sort_order => {enum, [asc, desc]}
    }.


-spec get_info(nkadmin:admin()) ->
    nkadmin:admin().

get_info(Admin) ->
    Fields = [obj_id, frame, tree, detail],
    maps:with([Fields], Admin).





