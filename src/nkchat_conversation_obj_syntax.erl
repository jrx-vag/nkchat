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
api('', get, Syntax) ->
    Syntax#{
        id => binary
    };

api('', create, Syntax) ->
    Syntax2 = Syntax#{
        obj_name => binary,
        description => binary,
        domain => binary
    },
    nklib_syntax:add_mandatory([obj_name, description], Syntax2);

api('', delete, Syntax) ->
    Syntax#{
        id => binary,
        reason => binary,
        delete_childs => boolean
    };

api('', update, Syntax) ->
    Syntax2 = Syntax#{
        id => binary,
        decription => binary
    },
    nklib_syntax:add_mandatory([description], Syntax2);

api('', add_members, Syntax) ->
    Syntax2 = Syntax#{
        id => binary,
        member_ids => {list, binary}
    },
    nklib_syntax:add_mandatory([member_ids], Syntax2);

api('', remove_members, Syntax) ->
    Syntax2 = Syntax#{
        id => binary,
        member_ids => {list, binary}
    },
    nklib_syntax:add_mandatory([id, member_ids], Syntax2);

api('', get_messages, Syntax) ->
    Syntax#{
        id => binary,
        size => integer,
        from => integer,
        start_date => integer
    };

api(_Sub, _Cmd, Syntax) ->
    lager:error("unknown syntax: ~p, ~p", [_Sub, _Cmd]),
    Syntax.
