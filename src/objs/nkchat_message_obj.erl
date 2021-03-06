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

%% @doc Message Object
-module(nkchat_message_obj).
-behavior(nkdomain_obj).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([create/4]).
-export([object_get_info/0, object_mapping/0, object_syntax/1,
         object_api_syntax/3, object_api_allow/4, object_api_cmd/4, object_event/2]).

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
                created_by => AuthorId,
                parent => ConvObjId,
                type_obj => Message
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
        type => ?CHAT_MESSAGE,
        dont_update_on_disabled => true,
        dont_delete_on_disabled => true
    }.


%% @private
object_mapping() ->
    #{
        text => #{
            type => text,
            fields => #{keyword => #{type=>keyword}}
        },
        file_id => #{type => keyword}
    }.


%% @private
object_syntax(_) ->
    #{
        text => binary,
        file_id => binary
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
object_event(Event, #obj_session{parent_id=ParentId, obj_id=ObjId, obj=Obj}=Session) ->
    case Event of
        created ->
            Msg = maps:with([obj_id, created_by, created_time, ?CHAT_MESSAGE], Obj),
            ok = nkchat_conversation_obj:message_event(ParentId, {created, Msg});
        deleted ->
            ok = nkchat_conversation_obj:message_event(ParentId, {deleted, ObjId});
        {updated, _} ->
            Msg = maps:with([obj_id, updated_time, ?CHAT_MESSAGE], Obj),
            ok = nkchat_conversation_obj:message_event(ParentId, {updated, Msg});
        _ ->
            ok
    end,
    {ok, Session}.



%% ===================================================================
%% Internal
%% ===================================================================
