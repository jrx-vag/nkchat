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

-export([create/4]).
-export([object_get_info/0, object_mapping/0, object_syntax/1,
         object_api_syntax/3, object_api_allow/4, object_api_cmd/4]).

-include("nkchat.hrl").


%% ===================================================================
%% Types
%% ===================================================================



%% @doc
%% Data must follow object's syntax
-spec create(nkservice:id(), nkdomain:name(), nkdomain:id(), binary()) ->
    {ok, nkdomain:obj_id(), nkdomain:path(), pid()} | {error, term()}.

create(Srv, ConvId, AuthorId, Message) ->
    Opts = #{father=>ConvId, referred_id=>AuthorId},
    Base = #{
        ?CHAT_MESSAGE => #{message => nklib_util:to_binary(Message)}
    },
    case nkdomain_obj_lib:make_obj(Srv, ?CHAT_MESSAGE, Base, Opts) of
        {ok, Obj} ->
            lager:error("CREATE: ~p", [Obj]),
            case nkdomain:create(Srv, Obj, #{}) of
                {ok, ?CHAT_MESSAGE, ObjId, Path, Pid} ->
                    {ok, ObjId, Path, Pid};
                {error, Error} ->
                    {error, Error}
            end;        {error, Error} ->
        {error, Error}
    end.


%% ===================================================================
%% nkdomain_obj behaviour
%% ===================================================================


%% @private
object_get_info() ->
    #{
        type => ?CHAT_MESSAGE
    }.


%% @private
object_mapping() ->
    #{
        message => #{
            type => text,
            fields => #{keyword => #{type=>keyword}}
        }
    }.


%% @private
object_syntax(load) ->
    #{
        message => binary
    };

object_syntax(update) ->
    #{
        message => binary
    }.


%% @private
object_api_syntax(Sub, Cmd, Syntax) ->
    nkchat_message_obj_syntax:api(Sub, Cmd, Syntax).


%% @private
object_api_allow(_Sub, _Cmd, _Data, State) ->
    {true, State}.


%% @private
object_api_cmd(Sub, Cmd, Data, State) ->
    nkchat_message_obj_api:cmd(Sub, Cmd, Data, State).




%% ===================================================================
%% Internal
%% ===================================================================


