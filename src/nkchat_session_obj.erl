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
-export([object_init/1, object_save/1, object_sync_op/3, object_async_op/2, object_handle_info/2]).
-export([find_unread/2]).

-include("nkchat.hrl").
-include_lib("nkdomain/include/nkdomain.hrl").
-include_lib("nkdomain/include/nkdomain_debug.hrl").


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


-record(?MODULE, {
    convs = #{} :: #{nkdomain:obj_id() => map()},
    active :: undefined | nkdomain:obj_id(),
    unread = #{} :: #{ConvId::nkdomain:obj_id() => integer()},
    disabled = #{} :: #{ConvId::nkdomain:obj_id() => boolean()},
    pids = #{} :: #{ConvId::nkdomain:obj_id() => pid()},
    meta = #{} :: map()
}).



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
    ChatSess = load_convs(Convs, #?MODULE{}, Session),
    {ok, Session#obj_session{data=ChatSess}}.


%% @private Prepare the object for saving
object_save(#obj_session{obj=Obj, data=ChatSess}=Session) ->
    #?MODULE{convs=ConvsMap} = ChatSess,
    ChatObj = #{
        conversations => maps:values(ConvsMap)
    },
    Obj2 = ?ADD_TO_OBJ(?CHAT_SESSION, ChatObj, Obj),
    {ok, Session#obj_session{obj=Obj2}};

object_save(Session) ->
    {ok, Session}.


%% @private
object_sync_op({?MODULE, get_convs}, _From, #obj_session{obj_id=ObjId, data=ChatSess}=Session) ->
    List = get_conv_list(ChatSess),
    {reply, {ok, ObjId, #{conversations=>List}}, Session};

object_sync_op({?MODULE, set_active_conv, ConvId}, _From, Session) ->
    case find_conv(ConvId, Session) of
        {ok, Conv} ->
            #obj_session{data=ChatSess} = Session,
            ChatSess2 = update_conv(ConvId, Conv, set_active, ChatSess),
            Session2 = Session#obj_session{data=ChatSess2, is_dirty=true},
            {reply, ok, Session2};
        not_found ->
            {reply, {error, conversation_not_found}, Session}
    end;

object_sync_op({?MODULE, add_conv, ConvId}, _From, #obj_session{srv_id=SrvId}=Session) ->
    case find_conv(ConvId, Session) of
        {ok, _Conv} ->
            {reply, {error, conversation_already_exists}, Session};
        not_found ->
            case nkdomain_obj_lib:load(SrvId, ConvId, #{register=>{?MODULE, self()}}) of
                #obj_id_ext{obj_id=ObjId, pid=Pid} ->
                    Conv = #{
                        obj_id => ObjId,
                        last_active_time => 0,
                        last_read_message_id => <<>>,
                        last_read_message_time => 0
                    },
                    #obj_session{data=ChatSess} = Session,
                    ChatSess2 = add_conv(ConvId, Conv, Pid, ChatSess),
                    Session2 = Session#obj_session{data=ChatSess2, is_dirty=true},
                    {reply, {ok, ConvId}, Session2};
                {error, object_not_found} ->
                    {reply, {error, conversation_not_found}, Session};
                {error, Error} ->
                    {reply, {error, Error}, Session}
            end
    end;

object_sync_op({?MODULE, rm_conv, ConvId}, _From, Session) ->
    case find_conv(ConvId, Session) of
        {ok, _Conv} ->
            nkdomain_obj:unregister(ConvId, {?MODULE, self()}),
            #obj_session{data=ChatSess} = Session,
            ChatSess2 = rm_conv(ConvId, ChatSess),
            Session2 = Session#obj_session{data=ChatSess2, is_dirty=true},
            {reply, ok, Session2};
        not_found ->
            {reply, {error, conversation_not_found}, Session}
    end;

object_sync_op(_Op, _From, _Session) ->
    continue.


%% @private
object_async_op({?MODULE, set_last_msg, ConvId, MsgId, Time}, Session) ->
    case find_conv(ConvId, Session) of
        {ok, Conv} ->
            Session2 = update_conv(ConvId, Conv, {last_msg, MsgId, Time}, Session),
            {noreply, Session2};
        _ ->
            {noreply, Session}
    end;

object_async_op(_Op, _Session) ->
    continue.


%% @private
object_handle_info({'DOWN', _Ref, process, Pid}, #obj_session{data=ChatSess}=Session) ->
    #?MODULE{convs=Convs, pids=Pids, disabled=Disabled} = ChatSess,
    case maps:find(Pid, Pids) of
        {ok, ConvId} ->
            ChatSess2 = case maps:is_key(ConvId, Convs) of
                false ->
                    ChatSess#?MODULE{
                        pids = maps:remove(pid, Pids)
                    };
                true ->
                    ?LLOG(notice, "conversation ~s is disabled", [ConvId], Session),
                    ChatSess#?MODULE{
                        pids = maps:remove(pid, Pids),
                        disabled = Disabled#{ConvId => true}
                    }
            end,
            {noreply, Session#obj_session{data=ChatSess2}};
        error ->
            continue
    end.


%% ===================================================================
%% Internal
%% ===================================================================


%% @private
load_convs([], ChatSess, _Session) ->
    ChatSess;

load_convs([#{obj_id:=ObjId}=Conv|Rest], ChatSess, #obj_session{srv_id=SrvId}=Session) ->
    ChatSess2 = case nkdomain_obj_lib:load(SrvId, ObjId, #{register=>{?MODULE, self()}}) of
        #obj_id_ext{pid=Pid} ->
            add_conv(ObjId, Conv, Pid, ChatSess);
        {error, _Error} ->
            #?MODULE{convs=Convs, disabled=Disabled} = ChatSess,
            Convs2 = Convs#{ObjId => Conv#{disabled=>true}},
            Disabled2 = Disabled#{ObjId => true},
            ChatSess#?MODULE{convs=Convs2, disabled=Disabled2}
    end,
    load_convs(Rest, ChatSess2, Session).


%% @private
reload_disabled(#obj_session{srv_id=SrvId, data=ChatSess}=Session) ->
    #?MODULE{convs=Convs, disabled=Disabled} = ChatSess,
    ChatSess2 = lists:foldl(
        fun(ConvId, Acc) ->
            case nkdomain_obj_lib:load(SrvId, ConvId, #{register=>{?MODULE, self()}}) of
                #obj_id_ext{pid=Pid} ->
                    #?MODULE{disabled=Disabled2} = Acc,
                    Acc2 = Acc#?MODULE{disabled=maps:remove(ConvId, Disabled2)},
                    Conv = maps:get(ConvId, Convs),
                    add_conv(ConvId, Conv, Pid, Acc2);
                {error, _Error} ->
                    Acc
            end
        end,
        ChatSess,
        maps:to_list(Disabled)),
    Session#obj_session{data=ChatSess2}.


%% @private
find_conv(ConvId, #obj_session{data=#?MODULE{convs=ConvsMap}}) ->
    case maps:find(ConvId, ConvsMap) of
        {ok, Conv} ->
            {ok, Conv};
        error ->
            not_found
    end.


%% @private
add_conv(ConvId, Conv, Pid, ChatSess) ->
    #?MODULE{convs=Convs, pids=Pids} = ChatSess,
    Convs2 = Convs#{ConvId => Conv},
    Pids2 = case maps:find(Pid, Pids) of
        true ->
            Pids;
        false ->
            monitor(process, Pid),
            Pids#{Pid => ConvId}
    end,
    ChatSess#?MODULE{convs=Convs2, pids=Pids2}.


%% @private
rm_conv(ConvId, ChatSess) ->
    #?MODULE{convs=Convs, disabled=Disabled} = ChatSess,
    ChatSess#?MODULE{
        convs = maps:remove(ConvId, Convs),
        disabled = maps:remove(ConvId, Disabled)
    }.


%% @private
update_conv(ConvId, Conv, Update, ChatSess) ->
    #?MODULE{convs=Convs} = ChatSess,
    case Update of
        set_active ->
            Conv2 = Conv#{last_active_time=>nklib_util:m_timestamp()},
            ChatSess#?MODULE{
                convs = Convs#{ConvId => Conv2},
                active = ConvId
            };
        {last_msg, MsgId, Time} ->
            Conv2 = Conv#{
                last_read_message_id => MsgId,
                last_read_message_time => Time
            },
            ChatSess#?MODULE{
                convs = Convs#{ConvId => Conv2}
            }
    end.


%% @private
get_conv_list(#?MODULE{convs=Convs}) ->
    List1 = lists:map(
        fun({_ObjId, #{last_active_time:=Time}=Conv}) -> {Time, Conv} end,
        maps:to_list(Convs)
    ),
    List2 = lists:keysort(1, List1),
    [Conv || {_, Conv} <- lists:reverse(List2)].


find_unread(Conv, #{srv_id:=SrvId}) ->
    #{obj_id:=ConvId, last_read_message_id:=_MsgId, last_read_message_time:=Time} = Conv,
    Search = #{
        filters => #{
            type => ?CHAT_MESSAGE,
            parent_id => ConvId,
            created_time => {Time, none}
        },
        size => 0
    },
    nkdomain_store:find(SrvId, Search).



