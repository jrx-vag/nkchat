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

-export([create/4, update/3]).
-export([object_info/0, object_es_mapping/0, object_parse/3, object_create/2, object_event/2]).
-export([object_admin_info/0]).
-export([syntax_check_file/3]).

-include("nkchat.hrl").
-include_lib("nkdomain/include/nkdomain.hrl").
-include_lib("nkdomain/include/nkdomain_debug.hrl").


%% ===================================================================
%% Public
%% ===================================================================

create(SrvId, DomainId, ConvId, Text) ->
    Obj = #{
        type => ?CHAT_MESSAGE,
        domain_id => DomainId,
        parent_id => ConvId,
        created_by => <<"admin">>,
        ?CHAT_MESSAGE => #{
            text => Text
        }
    },
    object_create(SrvId, Obj).


update(SrvId, MsgId, Text) ->
    nkdomain:update(SrvId, MsgId, #{?CHAT_MESSAGE => #{text => Text}}).



%% ===================================================================
%% nkdomain_obj behaviour
%% ===================================================================


%% @private
object_info() ->
    #{
        type => ?CHAT_MESSAGE,
        dont_update_on_disabled => true,
        dont_delete_on_disabled => true
    }.


%% @doc
object_admin_info() ->
    #{
        class => resource,
        weight => 2001,
        get_tree_detail => fun nkchat_message_obj_ui:table/1
    }.


%% @private
object_es_mapping() ->
    #{
        text => #{type => text},
        file_id => #{type => keyword}
    }.



%% @private
object_parse(_SrvId, _, _Obj) ->
    #{
        text => binary,
        file_id => fun ?MODULE:syntax_check_file/3,
        '__mandatory' => [text]
    }.



%% @doc
object_create(SrvId, #{parent_id:=_ConvId}=Obj) ->
    nkdomain_obj_make:create(SrvId, Obj);

object_create(_SrvId, _Obj) ->
    {error, {missing_field, <<"parent_id">>}}.


%% @private
object_event(Event, #?STATE{id=#obj_id_ext{srv_id=SrvId, obj_id=ObjId}, obj=Obj}=Session) ->
    #{parent_id:=ConvId} = Obj,
    case Event of
        created ->
            Msg = maps:with([obj_id, created_by, created_time, ?CHAT_MESSAGE], Obj),
            ok = nkchat_conversation_obj:message_event(SrvId, ConvId, {created, Msg});
        deleted ->
            ok = nkchat_conversation_obj:message_event(SrvId, ConvId, {deleted, ObjId});
        {updated, _} ->
            Msg = maps:with([obj_id, created_by, created_time, updated_time, ?CHAT_MESSAGE], Obj),
            ok = nkchat_conversation_obj:message_event(SrvId, ConvId, {updated, Msg});
        _ ->
            ok
    end,
    {ok, Session}.





%% ===================================================================
%% Internal
%% ===================================================================

syntax_check_file(file_id, File, Ctx) ->
    #{domain_srv_id:=SrvId} = Ctx,
    case nkdomain_lib:find(SrvId, File) of
        #obj_id_ext{type=?DOMAIN_FILE, obj_id=FileId} ->
            {ok, FileId};
        _ ->
            {error, {file_not_found, File}}
    end.
