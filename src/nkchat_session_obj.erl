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

%% @doc Session Object
-module(nkchat_session_obj).
-behavior(nkdomain_obj).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([create/2, find/2, start/3, stop/2]).
-export([set_active_conversation/3, set_last_message/5, add_conversation/3, remove_conversation/3]).
-export([object_get_info/0, object_mapping/0, object_syntax/1,
         object_api_syntax/3, object_api_allow/4, object_api_cmd/4]).
-export([object_init/1, object_stop/2, object_sync_op/3, object_async_op/2]).

-include("nkchat.hrl").
-include_lib("nkdomain/include/nkdomain.hrl").



%% ===================================================================
%% Public
%% ===================================================================

%% @doc
create(Srv, User) ->
    case nkdomain_obj_lib:find(Srv, User) of
        #obj_id_ext{obj_id = UserId} ->
            Opts = #{
                type_obj => #{conversations => []}
            },
            nkdomain_obj_lib:make_and_create(Srv, UserId, ?CHAT_SESSION, Opts);
        _ ->
            {error, user_not_found}
    end.


%% Data must follow object's syntax
-spec find(nkservice:id(), nkdomain:id()) ->
    {ok, nkdomain:obj_id()} | {error, term()}.

find(Srv, User) ->
    case nkdomain_obj_lib:find(Srv, User) of
        #obj_id_ext{obj_id=UserId} ->
            Search = #{
                filters => #{
                    type => ?CHAT_SESSION,
                    parent_id => UserId
                },
                sort => [#{created_time => #{order => desc}}]
            },
            case nkdomain_store:find(Srv, Search) of
                {ok, _, [], _Meta} ->
                    {error, session_not_found};
                {ok, _, [#{<<"obj_id">>:=ObjId}|_], _Meta} ->
                    {ok, ObjId};
                {error, Error} ->
                    {error, Error}
            end;
        _ ->
            {error, user_not_found}
    end.


%% @doc
start(Srv, Id, Caller) ->
    case nkdomain_obj_lib:load(Srv, Id, #{}) of
        #obj_id_ext{pid=Pid} ->
            nkdomain_obj:register(Pid, {?MODULE, Caller}),
            nkdomain_obj:sync_op(Pid, {?MODULE, get_convs});
        {error, Error} ->
            {error, Error}
    end.


%% @doc
stop(Srv, Id) ->
    case nkdomain_obj_lib:find(Srv, Id) of
        #obj_id_ext{pid=Pid} ->
            nkdomain_obj:unload(Pid, user_stop);
        _ ->
            {error, session_not_found}
    end.


%% @doc
set_active_conversation(Srv, Id, ConvId) ->
    case nkdomain_obj_lib:load(Srv, Id, #{}) of
        #obj_id_ext{pid=Pid} when is_pid(Pid) ->
            nkdomain_obj:sync_op(Pid, {?MODULE, set_active_conv, ConvId});
        {error, object_not_found} ->
            {error, session_not_found};
        {error, Error} ->
            {error, Error}
    end.


%% @doc
set_last_message(Srv, Id, ConvId, MsgId, Time) ->
    case nkdomain_obj_lib:load(Srv, Id, #{}) of
        #obj_id_ext{pid=Pid} when is_pid(Pid) ->
            nkdomain_obj:async_op(Pid, {?MODULE, set_last_msg, ConvId, MsgId, Time});
        {error, object_not_found} ->
            {error, session_not_found};
        {error, Error} ->
            {error, Error}
    end.


%% @doc
add_conversation(Srv, Id, ConvId) ->
    case nkdomain_obj_lib:load(Srv, Id, #{}) of
        #obj_id_ext{srv_id=SrvId, pid=Pid} when is_pid(Pid) ->
            case nkdomain_obj_lib:find(SrvId, ConvId) of
                #obj_id_ext{type = ?CHAT_CONVERSATION, obj_id=ConvId2} ->
                    nkdomain_obj:sync_op(Pid, {?MODULE, add_conv, ConvId2});
                #obj_id_ext{} ->
                    {error, conversation_not_found};
                {error, path_not_found} ->
                    {error, conversation_not_found};
                {error, object_not_found} ->
                    {error, conversation_not_found};
                {error, Error} ->
                    {error, Error}
            end;
        {error, object_not_found} ->
            {error, session_not_found};
        {error, Error} ->
            {error, Error}
    end.


%% @doc
remove_conversation(Srv, Id, ConvId) ->
    case nkdomain_obj_lib:load(Srv, Id, #{}) of
        #obj_id_ext{pid=Pid} when is_pid(Pid) ->
            nkdomain_obj:sync_op(Pid, {?MODULE, rm_conv, ConvId});
        {error, object_not_found} ->
            {error, session_not_found};
        {error, Error} ->
            {error, Error}
    end.


%% ===================================================================
%% nkdomain_obj behaviour
%% ===================================================================


%% @private
object_get_info() ->
    #{
        type => ?CHAT_SESSION
    }.


%% @private
object_mapping() ->
    #{
        conversations => #{
            type => object,
            dynamic => false,
            properties => #{
                obj_id => #{type => keyword},
                last_active_time => #{type => date},
                last_read_message_id => #{type => keyword},
                last_read_message_time => #{type => date}
            }
        }
    }.


%% @private
object_syntax(_) ->
    #{
        conversations =>
            {list,
                {syntax, #{
                    obj_id => binary,
                    last_active_time => integer,
                    last_read_message_id => binary,
                    last_read_message_time => integer,
                    '__mandatory' => [
                        <<?CHAT_SESSION/binary, ".conversations.obj_id">>,
                        <<?CHAT_SESSION/binary, ".conversations.last_active_time">>,
                        <<?CHAT_SESSION/binary, ".conversations.last_read_message_id">>,
                        <<?CHAT_SESSION/binary, ".conversations.last_read_message_time">>
                    ]
                }}}
    }.


%% @private
object_api_syntax(Sub, Cmd, Syntax) ->
    nkchat_session_obj_syntax:api(Sub, Cmd, Syntax).


%% @private
object_api_allow(_Sub, _Cmd, _Data, State) ->
    {true, State}.


%% @private
object_api_cmd(Sub, Cmd, Data, State) ->
    nkchat_session_obj_api:cmd(Sub, Cmd, Data, State).



%% @private When the object is loaded, we make our cache
object_init(#obj_session{obj=Obj}=Session) ->
    #{?CHAT_SESSION := #{conversations:=Convs}} = Obj,
    ConvsMap1 = lists:map(fun(#{obj_id:=ObjId}=Conv) -> {ObjId, Conv} end, Convs),
    ConvsMap2 = maps:from_list(ConvsMap1),
    {ok, Session#obj_session{internal_obj=ConvsMap2}}.


%% @private When stopping, we prepare the object for saving
object_stop(_Reason, #obj_session{obj=Obj, internal_obj=ConvsMap}=Session) ->
    #{?CHAT_SESSION := ChatSession} = Obj,
    ChatSession2 = ChatSession#{conversations:=maps:values(ConvsMap)},
    Obj2 = ?ADD_TO_OBJ(?CHAT_SESSION, ChatSession2, Obj),
    {ok, Session#obj_session{obj=Obj2, is_dirty=true}}.


%% @private
object_sync_op({?MODULE, get_convs}, _From, Session) ->
    List = get_conv_list(Session),
    {reply, {ok, List}, Session};

object_sync_op({?MODULE, set_active_conv, ConvId}, _From, Session) ->
    case find_conv3(ConvId, Session) of
        {ok, Conv} ->
            Session2 = update_conv(ConvId, Conv, set_active, Session),
            {reply, ok, Session2};
        not_found ->
            {reply, {error, conversation_not_found}, Session}
    end;

object_sync_op({?MODULE, add_conv, ConvId}, _From, #obj_session{srv_id=SrvId}=Session) ->
    case find_conv3(ConvId, Session) of
        {ok, _Conv} ->
            {reply, {error, conversation_already_exists}, Session};
        not_found ->
            case nkdomain_obj_lib:load(SrvId, ConvId, #{register=>{?MODULE, self()}}) of
                #obj_id_ext{obj_id=ObjId} ->
                    Conv = #{
                        obj_id => ObjId,
                        last_active_time => 0,
                        last_read_message_id => <<>>,
                        last_read_message_time => 0
                    },
                    Session2 = add_conv(ConvId, Conv, Session),
                    {reply, {ok, ConvId}, Session2};
                {error, object_not_found} ->
                    {reply, {error, conversation_not_found}, Session};
                {error, Error} ->
                    {reply, {error, Error}, Session}
            end
    end;

object_sync_op({?MODULE, rm_conv, ConvId}, _From, Session) ->
    case find_conv3(ConvId, Session) of
        {ok, _Conv} ->
            Session2 = rm_conv(ConvId, Session),
            nkdomain_obj:unregister(ConvId, {?MODULE, self()}),
            {reply, ok, Session2};
        not_found ->
            {reply, {error, conversation_not_found}, Session}
    end;

object_sync_op(_Op, _From, _Session) ->
    continue.


%% @private
object_async_op({?MODULE, set_last_msg, ConvId, MsgId, Time}, Session) ->
    case find_conv3(ConvId, Session) of
        {ok, Conv} ->
            Session2 = update_conv(ConvId, Conv, {last_msg, MsgId, Time}, Session),
            {noreply, Session2};
        _ ->
            {noreply, Session}
    end;

object_async_op(_Op, _Session) ->
    continue.



%% ===================================================================
%% Internal
%% ===================================================================


%% @private
find_conv3(ConvId, #obj_session{internal_obj=ConvsMap}) ->
    case maps:find(ConvId, ConvsMap) of
        {ok, Conv} ->
            {ok, Conv};
        error ->
            not_found
    end.


%% @private
add_conv(ConvId, Conv, #obj_session{internal_obj=ConvsMap}=Session) ->
    ConvsMap2 = ConvsMap#{ConvId => Conv},
    Session#obj_session{internal_obj=ConvsMap2}.


%% @private
rm_conv(ConvId, #obj_session{internal_obj=ConvsMap}=Session) ->
    ConvsMap2 = maps:remove(ConvId, ConvsMap),
    Session#obj_session{internal_obj=ConvsMap2}.


%% @private
update_conv(ConvId, Conv, Update, #obj_session{internal_obj=ConvsMap}=Session) ->
    Conv2 = case Update of
        set_active ->
            Conv#{last_active_time=>nklib_util:m_timestamp()};
        {last_msg, MsgId, Time} ->
            Conv#{
                last_read_message_id => MsgId,
                last_read_message_time => Time
            }
    end,
    ConvsMap2 = ConvsMap#{ConvId => Conv2},
    Session#obj_session{internal_obj=ConvsMap2}.


%% @private
get_conv_list(#obj_session{internal_obj=ConvsMap}) ->
    List1 = lists:map(
        fun({_ObjId, #{last_active_time:=Time}=Conv}) -> {Time, Conv} end,
        maps:to_list(ConvsMap)
    ),
    List2 = lists:keysort(1, List1),
    [Conv || {_, Conv} <- lists:reverse(List2)].