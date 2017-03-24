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

%% @doc Message Object
-module(nkchat_message_obj).
-behavior(nkdomain_obj).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([object_get_info/0, object_mapping/0, object_syntax/1,
         object_api_syntax/3, object_api_allow/4, object_api_cmd/4]).

-include("nkchat.hrl").


%% ===================================================================
%% Types
%% ===================================================================


%% ===================================================================
%% nkdomain_obj behaviour
%% ===================================================================


%% @private
object_get_info() ->
    #{
        type => ?CHAT_CONVERSATION
    }.


%% @private
object_mapping() ->
    #{
        member_ids => #{type => keyword}
    }.


%% @private
object_syntax(_) ->
    #{
        member_ids => {list, binary}
    }.


%% @private
object_api_syntax(Sub, Cmd, Syntax) ->
    nkchat_conversation_obj_syntax:api(Sub, Cmd, Syntax).


%% @private
object_api_allow(_Sub, _Cmd, _Data, State) ->
    {true, State}.


%% @private
object_api_cmd(Sub, Cmd, Data, State) ->
    nkchat_conversation_obj_api:cmd(Sub, Cmd, Data, State).




%% ===================================================================
%% Internal
%% ===================================================================


