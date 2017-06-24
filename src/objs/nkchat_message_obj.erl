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

-include("nkchat.hrl").
-include_lib("nkdomain/include/nkdomain.hrl").
-include_lib("nkdomain/include/nkdomain_debug.hrl").


%% ===================================================================
%% Public
%% ===================================================================

create(SrvId, DomainId, ConvId, Text) ->
    Obj = #{
        type => ?CHAT_MESSAGE,
        parent_id => DomainId,
        created_by => <<"admin">>,
        ?CHAT_MESSAGE => #{
            conversation_id => ConvId,
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
        conversation_id => #{type => keyword},
        text => #{type => text},
        file_id => #{type => keyword}
    }.



%% @private
object_parse(_SrvId, update, _Obj) ->
    #{
        text => binary,
        file_id => binary
    };

object_parse(_SrvId, load, _Obj) ->
    #{
        conversation_id => binary,
        text => binary,
        file_id => binary,
        '__mandatory' => [text, conversation_id]
    }.


%% @doc
object_create(SrvId, Obj) ->
    #{?CHAT_MESSAGE:=#{conversation_id := ConvId}} = Obj,
    Obj2 = Obj#{referred_id=>ConvId},
    case nkdomain_obj_make:create(SrvId, Obj2) of
        {ok, ObjIdExt, Unknown} ->
            {ok, ObjIdExt, Unknown};
        {error, referred_not_found} ->
            {error, conversation_not_found};
        {error, Error} ->
            {error, Error}
    end.


%% @private
object_event(Event, #?STATE{id=#obj_id_ext{srv_id=SrvId, obj_id=ObjId}, obj=Obj}=Session) ->
    #{referred_id:=ConvId} = Obj,
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
