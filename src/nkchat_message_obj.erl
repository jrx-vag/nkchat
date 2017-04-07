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
         object_api_syntax/3, object_api_allow/4, object_api_cmd/4,
         object_start/1, object_deleted/2, object_updated/2]).

-include("nkchat.hrl").
-include_lib("nkdomain/include/nkdomain.hrl").
-include_lib("nkdomain/include/nkdomain_debug.hrl").


%% ===================================================================
%% Types
%% ===================================================================



%% @doc
%% Data must follow object's syntax
-spec create(nkservice:id(), nkdomain:name(), nkdomain:id(), binary()) ->
    {ok, nkdomain:obj_id(), nkdomain:path(), pid()} | {error, term()}.

create(Srv, ConvId, AuthorId, Message) ->
    case nkdomain_obj_lib:load(Srv, ConvId, #{}) of
        #obj_id_ext{type = ?CHAT_CONVERSATION, obj_id=ConvObjId} ->
            Opts = #{
                referred_id => AuthorId,
                parent => ConvObjId,
                type_obj => #{message => nklib_util:to_binary(Message)}
            },
            nkdomain_obj_lib:make_and_create(Srv, ConvId, ?CHAT_MESSAGE, Opts);
        {error, object_not_found} ->
            {error, conversation_not_found};
        {error, Error} ->
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


%% @private
object_start(#obj_session{parent_pid=Pid, obj_id=ObjId, obj=Obj}=Session) ->
    #{?CHAT_MESSAGE:=Msg} = Obj,
    ok = nkchat_conversation_obj:message_created(Pid, ObjId, Msg),
    {ok, Session}.


%% @private
object_deleted(_Reason, #obj_session{parent_pid=Pid, obj_id=ObjId}=Session) ->
    ok = nkchat_conversation_obj:message_deleted(Pid, ObjId),
    {ok, Session}.


%% @private
object_updated(_Update, #obj_session{parent_pid=Pid, obj_id=ObjId, obj=Obj}=Session) ->
    #{?CHAT_MESSAGE:=Msg} = Obj,
    ok = nkchat_conversation_obj:message_updated(Pid, ObjId, Msg),
    {ok, Session}.



%% ===================================================================
%% Internal
%% ===================================================================


