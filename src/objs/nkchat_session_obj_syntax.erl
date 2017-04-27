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

-export([api/3]).



%% ===================================================================
%% Syntax
%% ===================================================================


%% @doc
api('', find, Syntax) ->
    Syntax#{
        user_id => binary
    };

api('', create, Syntax) ->
    Syntax#{
        user_id => binary,
        events => {list, binary}
    };

api('', start, Syntax) ->
    Syntax2 = Syntax#{
        id => binary,
        events => {list, binary}
    },
    nklib_syntax:add_mandatory([id], Syntax2);

api('', stop, Syntax) ->
    Syntax#{
        id => binary,
        reason => binary
    };

api('', get_all_conversations, Syntax) ->
    Syntax#{
        id => binary
    };

api('', get_conversation, Syntax) ->
    Syntax2 = Syntax#{
        id => binary,
        conversation_id => binary
    },
    nklib_syntax:add_mandatory([conversation_id], Syntax2);

api('', set_active_conversation, Syntax) ->
    Syntax2 = Syntax#{
        id => binary,
        conversation_id => binary
    },
    nklib_syntax:add_mandatory([conversation_id], Syntax2);

api('', add_conversation, Syntax) ->
    Syntax2 = Syntax#{
        id => binary,
        conversation_id => binary
    },
    nklib_syntax:add_mandatory([conversation_id], Syntax2);

api('', remove_conversation, Syntax) ->
    Syntax2 = Syntax#{
        id => binary,
        conversation_id => binary
    },
    nklib_syntax:add_mandatory([conversation_id], Syntax2);

api(Sub, Cmd, Syntax) ->
    nkdomain_obj_syntax:syntax(Sub, Cmd, Syntax).
