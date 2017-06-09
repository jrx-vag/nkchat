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

%% @doc State Object
-module(nkchat_session_obj).
-behavior(nkdomain_obj).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([create/2, find/2, start/3, stop/2, get_all_conversations/2, get_conversation/3]).
-export([set_active_conversation/3, add_conversation/3, remove_conversation/3]).
-export([conversation_event/6]).
-export([object_get_info/0, object_mapping/0, object_parse/3,
         object_api_syntax/2, object_api_allow/3, object_api_cmd/2]).
-export([object_init/1, object_start/1, object_stop/2, object_restore/1, object_send_event/2,
         object_sync_op/3, object_async_op/2, object_handle_info/2]).
-export([object_admin_info/0]).
-export([find_unread/2]).
-export_type([meta/0, event/0]).

-include("nkchat.hrl").
-include_lib("nkdomain/include/nkdomain.hrl").
-include_lib("nkdomain/include/nkdomain_debug.hrl").
-include_lib("nkevent/include/nkevent.hrl").


%% Period to find for inactive conversations
-define(CHECK_TIME, 5*60*1000).


%% ===================================================================
%% Types
%% ===================================================================


-type meta() ::
    #{
        user_id => nkdomain:obj_id()
    }.


-type event() ::
    {conversation_activated, ConvId::nkdomain:obj_id()} |
    {conversation_added, ConvId::nkdomain:obj_id()} |
    {conversation_removed, ConvId::nkdomain:obj_id()} |
    {member_added, ConvId::nkdomain:obj_id(), IsActive::boolean(), MemberId::nkdomain:obj_id()} |
    {member_removed, ConvId::nkdomain:obj_id(), IsActive::boolean(), MemberId::nkdomain:obj_id()} |
    {message_created, nkdomain:obj()} |
    {message_updated, nkdomain:obj()} |
    {message_deleted, nkdomain:obj_id()} |
    {unread_counter_updated, ConvId::nkdomain:obj_id(), integer()}.




%% ===================================================================
%% Public
%% ===================================================================

%% @doc
-spec create(nkservice:id(), nkdomain:id()) ->
    {ok, nkdomain_obj_lib:make_and_create_reply(), pid()} | {error, term()}.

create(Srv, User) ->
    case nkdomain_obj_lib:find(Srv, User) of
        #obj_id_ext{obj_id = UserId} ->
            Obj = #{
                type => ?CHAT_SESSION,
                parent_id => UserId,
                ?CHAT_SESSION => #{conversations => []}
            },
            nkdomain_obj_lib:make_and_create(Srv, <<>>, Obj, #{});
        _ ->
            {error, user_not_found}
    end.


%% @doc Tries to find a previous session for this user
-spec find(nkservice:id(), nkdomain:id()) ->
    {ok, [map()]} | {error, term()}.

find(Srv, User) ->
    case nkdomain_obj_lib:find(Srv, User) of
        #obj_id_ext{obj_id=UserId} ->
            Search = #{
                filters => #{
                    type => ?CHAT_SESSION,
                    parent_id => UserId
                },
                sort => [#{created_time => #{order => desc}}],
                fields => [created_time]
            },
            case nkdomain_store:find(Srv, Search) of
                {ok, _, [], _Meta} ->
                    {error, session_not_found};
                {ok, _, List, _Meta} ->
                    {ok, List};
                {error, Error} ->
                    {error, Error}
            end;
        _ ->
            {error, user_not_found}
    end.


%% @doc Starts a new session, connected to the Caller
start(Srv, Id, ApiServerPid) ->
    case nkdomain_obj_lib:load(Srv, Id, #{}) of
        #obj_id_ext{pid=Pid} ->
            nkdomain_obj:sync_op(Pid, {?MODULE, start, ApiServerPid});
        {error, Error} ->
            {error, Error}
    end.


%% @doc
stop(Srv, Id) ->
    nkdomain_obj_lib:unload(Srv, Id, user_stop, session_not_found).


%% @doc
add_conversation(Srv, Id, ConvId) ->
    case nkdomain_obj_lib:load(Srv, ConvId, #{}) of
        #obj_id_ext{type = ?CHAT_CONVERSATION, obj_id=ConvId2} ->
            sync_op(Srv, Id, {?MODULE, add_conv, ConvId2});
        {error, object_not_found} ->
            {error, conversation_not_found};
        {error, Error} ->
            {error, Error}
    end.


%% @doc
remove_conversation(Srv, Id, ConvId) ->
    sync_op(Srv, Id, {?MODULE, rm_conv, ConvId}).


%% @doc
get_all_conversations(Srv, Id) ->
    sync_op(Srv, Id, {?MODULE, get_all_conversations}).


%% @doc
get_conversation(Srv, Id, ConvId) ->
    sync_op(Srv, Id, {?MODULE, get_conversation, ConvId}).


%% @doc
-spec set_active_conversation(nkservice:id(), nkdomain:id(), nkdomain:id()) ->
    {ok, map()} | {error, term()}.

set_active_conversation(Srv, Id, ConvId) ->
    sync_op(Srv, Id, {?MODULE, set_active_conv, ConvId}).


%% @doc
-spec conversation_event(nkservice:id(), nkdomain:obj_id(), nkdomain:obj_id(),
                          nkdomain:obj_id(), term(), meta()) ->
    ok.

conversation_event(_SrvId, SessId, MemberId, ConvId, Event, Meta) ->
    case nkdomain_obj_lib:find_loaded(SessId) of
        #obj_id_ext{pid = Pid} ->
            parse_online_event(Pid, ConvId, Event);
        not_found ->
            parse_offline_event(SessId, ConvId, Event, MemberId, Meta)
    end.



%% ===================================================================
%% nkdomain_obj behaviour
%% ===================================================================

-type sess_conv() :: #{
    linked => boolean(),
    unread_count => integer(),
    last_message => nkdomain:obj_id(),
    last_message_time => nklib_util:m_timestamp()
}.


-record(?MODULE, {
    obj_convs :: #{nkdomain:obj_id() => map()},             % Conversation cache
    sess_convs :: #{nkdomain:obj_id() => sess_conv()},      % Internal data
    active_id :: undefined | nkdomain:obj_id(),
    api_pid :: pid(),
    meta = #{} :: map()
}).



%% @private
object_get_info() ->
    #{
        type => ?CHAT_SESSION

    }.


%% @doc
object_admin_info() ->
    #{
        class => session,
        weight => 2000,
        tree_id => <<"domain_tree_sessions_chat_sessions">>
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
                last_seen_message_time => #{type => date}
            }
        }
    }.


%% @private
object_parse(_SrvId, _Mode, _Obj) ->
    #{
        conversations =>
            {list,
                #{
                    obj_id => binary,
                    last_active_time => integer,
                    last_seen_message_time => integer,
                    '__mandatory' => [obj_id]
                }
            }
    }.


%% @private
object_api_syntax(Cmd, Syntax) ->
    nkchat_session_obj_syntax:api(Cmd, Syntax).


%% @private
object_api_allow(_Cmd, _Req, State) ->
    {true, State}.


%% @private
object_api_cmd(Cmd, Req) ->
    nkchat_session_obj_api:cmd(Cmd, Req).


%% @private
object_send_event(Event, State) ->
    nkchat_session_obj_events:event(Event, State).


%% @private
%% We initialize soon in case of early terminate
object_init(State) ->
    {ok, State#?NKOBJ{data=#?MODULE{}}}.


%% @private When the object is loaded, we make our cache
object_start(#?NKOBJ{obj=Obj}=State) ->
    #{?CHAT_SESSION := #{conversations := ConvsList}} = Obj,
    ObjConvs = maps:from_list([{Id, C} || #{obj_id:=Id}=C <- ConvsList]),
    SessConvs = lists:foldl(
        fun(ConvId, Acc) -> Acc#{ConvId => make_sess_conv(ConvId, State)} end,
        #{},
        maps:keys(ObjConvs)),
    Data = #?MODULE{
        obj_convs = ObjConvs,
        sess_convs = SessConvs
    },
    {ok, State#?NKOBJ{data=Data}}.


%% @private
object_stop(_Reason, State) ->
    {ok, nkdomain_obj_util:unlink_server_api(?MODULE, State)}.


%% @private Prepare the object for saving
object_restore(#?NKOBJ{obj = Obj, data = #?MODULE{} = Data} = State) ->
    #?MODULE{obj_convs = Convs} = Data,
    Obj2 = ?ADD_TO_OBJ(?CHAT_SESSION, #{conversations=>maps:values(Convs)}, Obj),
    {ok, State#?NKOBJ{obj = Obj2}}.


%% @private
object_sync_op({?MODULE, start, ApiPid}, From, State) ->
    #?NKOBJ{data=#?MODULE{api_pid=OldPid}} = State,
    case OldPid /= undefined andalso ApiPid /= OldPid of
        true ->
            {reply, {error, session_already_present}, State};
        false ->
            State2 = nkdomain_obj_util:link_server_api(?MODULE, ApiPid, State),
            object_sync_op({?MODULE, get_all_conversations}, From, State2)
    end;

object_sync_op({?MODULE, get_all_conversations}, _From, #?NKOBJ{obj_id=ObjId}=State) ->
    ConvIds = get_conv_ids(State),
    {Reply, State2} = get_convs_info(ConvIds, [], State),
    {reply, {ok, ObjId, #{conversations=>Reply}}, State2};

object_sync_op({?MODULE, get_conversation, ConvId}, _From, State) ->
    case get_conv_extra_info(ConvId, true, State) of
        {ok, Data, State2} ->
            {reply, {ok, Data}, State2};
        not_found ->
            {reply, {error, conversation_not_found}, State}
    end;

object_sync_op({?MODULE, set_active_conv, ConvId}, _From, State) ->
    case get_conv(ConvId, State) of
        {ok, ConvObjId, Conv, SessConv} ->
            case do_set_active_conv(ConvObjId, Conv, SessConv, State) of
                {ok, Reply, State2} ->
                    {reply_and_save, {ok, Reply}, State2};
                {error, Error} ->
                    {reply, {error, Error}, State}
            end;
        not_found ->
            {reply, {error, conversation_not_found}, State}
    end;

object_sync_op({?MODULE, add_conv, ConvId}, _From, State) ->
    case get_conv(ConvId, State) of
        not_found ->
            case do_add_conv(ConvId, State) of
                {ok, ConvObjId, State2} ->
                    {ok, Data, State3} = get_conv_extra_info(ConvObjId, false, State2),
                    {reply_and_save, {ok, Data}, State3};
                {error, Error} ->
                    {reply, {error, Error}, State}
            end;
        _ ->
            {reply, {error, conversation_is_already_member}, State}
    end;

object_sync_op({?MODULE, rm_conv, ConvId}, _From, State) ->
    case get_conv(ConvId, State) of
        {ok, ConvObjId, Conv, SessConv} ->
            {ok, Reply, State2} = do_rm_conv(ConvObjId, Conv, SessConv, State),
            {reply_and_save, Reply, State2};
        not_found ->
            {reply, {error, conversation_not_found}, State}
    end;

object_sync_op(_Op, _From, _State) ->
    continue.


%% @private
object_async_op({?MODULE, conversation_event, ConvId, Event}, State) ->
    case get_conv(ConvId, State) of
        {ok, ConvObjId, Conv, SessConv} ->
            IsActive = is_active(ConvId, State),
            do_conversation_event(Event, ConvObjId, IsActive, Conv, SessConv, State);
        not_found ->
            ?LLOG(notice, "received event for unknown conversation", [], State)
    end;

object_async_op(_Op, _State) ->
    continue.


object_handle_info(_Info, _State) ->
    continue.






%% ===================================================================
%% Internal
%% ===================================================================

%% @private
sync_op(Srv, Id, Op) ->
    nkdomain_obj_lib:sync_op(Srv, Id, ?CHAT_SESSION, Op, session_not_found).


%%%% @private
%%async_op(Srv, Id, Op) ->
%%    nkdomain_obj_lib:async_op(Srv, Id, ?CHAT_SESSION, Op, session_not_found).


%% @private
parse_online_event(Pid, ConvId, Event) ->
    % lager:warning("Chat session online for ~s: ~p", [ConvId, Event]),
    nkdomain_obj:async_op(Pid, {?MODULE, conversation_event, ConvId, Event}).


%% @private
parse_offline_event(_SessId, ConvId, Event, UserId, Meta) ->
    lager:warning("Chat session offline for ~s: ~p (~s, ~p)", [ConvId, Event, UserId, Meta]).


%% @private
do_set_active_conv(ConvId, Conv, SessConv, State) ->
    case nkdomain_obj:is_enabled(ConvId) of
        {ok, true} ->
            ?DEBUG("activated conversation ~s", [ConvId], State),
            {ok, Reply, State2} = get_conv_extra_info(ConvId, false, State),
            Now = nkdomain_util:timestamp(),
            Conv2 = Conv#{
                last_active_time => Now,
                last_seen_message_time => Now
            },
            SessConv2 = SessConv#{
                unread_count => 0
            },
            State3 = update_conv(ConvId, Conv2, SessConv2, State2),
            State4 = update_active(ConvId, State3),
            State5 = do_event({conversation_activated, ConvId}, State4),
            {ok, Reply, State5};
        {ok, false} ->
            {error, object_is_disabled}
    end.


%% @private
do_add_conv(ConvId, State) ->
    case link_conv(ConvId, State) of
        {ok, ConvObjId} ->
            % We don't want to count any older message as unread
            Conv = #{
                obj_id => ConvId,
                last_active_time => 0,
                last_seen_message_time => nkdomain_util:timestamp()
            },
            SessConv = make_sess_conv(ConvId, State),
            ?DEBUG("added conversation ~s", [ConvObjId], State),
            State2 = update_conv(ConvObjId, Conv, SessConv, State),
            State3 = do_event({conversation_added, ConvObjId}, State2),
            {ok, ConvObjId, State3};
        {error, Error} ->
            {error, Error}
    end.


%% @private
do_rm_conv(ConvId, _Conv, _SessConv, State) ->
    {Convs, SessConvs} = get_convs(State),
    Convs2 = maps:remove(ConvId, Convs),
    SessConvs2 = maps:remove(ConvId, SessConvs),
    State2 = update_convs(Convs2, SessConvs2, State),
    State3 = case is_active(ConvId, State) of
        true ->
            update_active(undefined, State2);
        false ->
            State2
    end,
    ?DEBUG("removed conversation ~s", [ConvId], State2),
    State4 = do_event({conversation_removed, ConvId}, State3),
    Reply = unlink_conv(ConvId, State4),
    {ok, Reply, State4}.


%% @private generates a sess_conv object
make_sess_conv(ConvId, State) ->
    Linked = case link_conv(ConvId, State) of
        {ok, ConvId} ->
            true;
        {error, Error} ->
            ?LLOG(notice, "could not load conversation ~s: ~p", [ConvId, Error], State),
            false
    end,
    LastMsg = find_last_message(ConvId, State),
    #{
        linked => Linked,
        unread_count => 0,
        last_message => LastMsg,
        last_message_time => maps:get(<<"created_time">>, LastMsg, 0)
    }.



%% @private
link_conv(ConvId, #?NKOBJ{srv_id=SrvId, obj_id=SessId, parent_id=UserId}) ->
    Opts = #{
        usage_link => {SessId, {?MODULE, SessId}}
    },
    case nkdomain_obj_lib:load(SrvId, ConvId, Opts) of
        #obj_id_ext{obj_id=ConvObjId, pid=Pid} ->
            case nkchat_conversation_obj:add_session(Pid, UserId, SessId, #{}) of
                ok ->
                    {ok, ConvObjId};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.


%% @private
unlink_conv(ConvId, #?NKOBJ{obj_id=SessId, parent_id=UserId}) ->
%%    Opts = #{
%%        usage_link => {SessId, {?MODULE, SessId}}
%%    },
    case nkchat_conversation_obj:remove_session(ConvId, UserId, SessId) of
        ok ->
            ok;
        {error, Error} ->
            {error, Error}
    end.



%% @private
do_conversation_event({member_added, MemberId}, ConvId, IsActive, _Conv, _SessConv, State) ->
    {noreply, do_event({member_added, ConvId, IsActive, MemberId}, State)};

do_conversation_event({member_removed, MemberId}, ConvId, IsActive, _Conv, _SessConv, State) ->
    State2 = do_event({member_removed, ConvId, IsActive, MemberId}, State),
    case State of
        #?NKOBJ{parent_id=MemberId} ->
            case get_conv(ConvId, State2) of
                {ok, ConvObjId, Conv, SessConv} ->
                    {ok, _Reply, State3} = do_rm_conv(ConvObjId, Conv, SessConv, State),
                    {noreply_and_save, State3};
                not_found ->
                    {noreply, State2}
            end;
        _ ->
            {noreply, State2}
    end;

do_conversation_event({message_created, Msg}, ConvId, true, Conv, SessConv, State) ->
    ?DEBUG("message event for active conversation", [], State),
    #{created_time:=Time} = Msg,
    Conv2 = Conv#{
        last_seen_message_time => Time
    },
    SessConv2 = SessConv#{
        unread_count := 0,
        last_message := Msg,
        last_message_time := Time
    },
    State2 = update_conv(ConvId, Conv2, SessConv2, State),
    {noreply, do_event({message_created, ConvId, Msg}, State2)};

do_conversation_event({message_created, Msg}, ConvId, false, Conv, SessConv, State) ->
    ?DEBUG("message event for not active conversation", [], State),
    #{created_time:=Time} = Msg,
    Count = maps:get(unread_count, SessConv, 0) + 1,
    SessConv2 = SessConv#{
        unread_count => Count,
        last_message => Msg,
        last_message_time => Time
    },
    State2 = update_conv(ConvId, Conv, SessConv2, State),
    {noreply, do_event({unread_counter_updated, ConvId, Count, Msg}, State2)};

do_conversation_event({message_updated, Msg}, ConvId, true, _Conv, _SessConv, State) ->
    {noreply, do_event({message_updated, ConvId, Msg}, State)};

do_conversation_event({message_deleted, MsgId}, ConvId, true, _Conv, _SessConv, State) ->
    {noreply, do_event({message_deleted, ConvId, MsgId}, State)};

do_conversation_event(_Event, _ConvId, _IsActive, _Conv, _SessConv, State) ->
    lager:notice("NOT PROCESSED EVENT: ~p", [_Event]),
    {noreply, State}.



%% @private
get_convs_info([], Acc, State) ->
    {Acc, State};

get_convs_info([ConvId|Rest], Acc, State) ->
    {ok, Data, State2} = get_conv_info(ConvId, true, State),
    get_convs_info(Rest, [Data|Acc], State2).


%% @private
get_conv_info(ConvId, GetUnread, State) ->
    case get_conv(ConvId, State) of
        {ok, ConvObjId, Conv, SessConv} ->
            ConvData1 = maps:with([last_active_time, last_seen_message_time], Conv),
            #{unread_count:=Count0, last_message:=LastMsg, last_message_time:=LastMsgTime} = SessConv,
            Count = case GetUnread of
                true ->
                    find_unread(Conv, State);
                false ->
                    Count0
            end,
            ConvData2 = ConvData1#{last_message => LastMsg, last_message_time => LastMsgTime, unread_count=>Count},
            State2 = case Count of
                Count0 ->
                    State;
                _ ->
                    update_conv(ConvObjId, Conv, SessConv#{unread_count:=Count}, State)
            end,
            case nkchat_conversation_obj:get_sess_info(ConvObjId) of
                {ok, Data1} ->
                    Data2 = case Data1 of
                        #{subtype:=[<<"one2one">>], member_ids:=MemberIds} ->
                            #?NKOBJ{parent_id=UserId, srv_id=SrvId} = State,
                            case MemberIds -- [UserId] of
                                [PeerId] ->
                                    case nkdomain_user_obj:get_name(SrvId, PeerId) of
                                        {ok, User} ->
                                            maps:remove(member_ids, Data1#{peer=>User});
                                        {error, _Error} ->
                                            Data1
                                    end;
                                _ ->
                                    Data1
                            end;
                        _ ->
                            Data1
                    end,
                    {ok, maps:merge(Data2, ConvData2), State2};
                {error, Error} ->
                    ?LLOG(notice, "could not read conversation ~s: ~p", [ConvObjId, Error], State2),
                    Data = ConvData2#{
                        is_enabled => false
                    },
                    {ok, Data, State2}
            end;
        not_found ->
            not_found
    end.


%% @private
get_conv_extra_info(ConvId, GetUnread, #?NKOBJ{srv_id=SrvId}=State) ->
    case get_conv_info(ConvId, GetUnread, State) of
        {ok, #{member_ids:=MemberIds}=Data, State2} ->
            Data2 = maps:remove(member_ids, Data),
            Members = lists:foldl(
                fun(UserId, Acc) ->
                    case nkdomain_user_obj:get_name(SrvId, UserId) of
                        {ok, User} -> [User|Acc];
                        {error, _Error} -> Acc
                    end
                end,
                [],
                MemberIds),
            {ok, Data2#{members=>Members}, State2};
        {ok, Data, State} ->
            {ok, Data, State};
        not_found ->
            not_found
    end.


%% @private
is_active(ConvId, #?NKOBJ{data=#?MODULE{active_id=Active}}) ->
    ConvId == Active.


%% @private
get_conv(ConvId, State) ->
    {Convs, SessConvs} = get_convs(State),
    case maps:find(ConvId, Convs) of
        {ok, Conv} ->
            {ok, ConvId, Conv, maps:get(ConvId, SessConvs)};
        error ->
            case nkdomain_obj_lib:find_loaded(ConvId) of
                #obj_id_ext{obj_id=ConvObjId} ->
                    case maps:find(ConvObjId, Convs) of
                        {ok, Conv} ->
                            {ok, ConvObjId, Conv, maps:get(ConvObjId, SessConvs)};
                        error ->
                            not_found
                    end;
                _ ->
                     not_found
            end
    end.


%% @private
get_conv_ids(#?NKOBJ{data=#?MODULE{obj_convs=Convs}}) ->
    maps:keys(Convs).


%% @private
get_convs(#?NKOBJ{data=#?MODULE{obj_convs=Convs, sess_convs=SessConvs}}) ->
    {Convs, SessConvs}.


%% @private
update_conv(ConvId, Conv, SessConv, #?NKOBJ{data=Data}=State) ->
    #?MODULE{obj_convs=Convs, sess_convs=SessConvs} = Data,
    Convs2 = Convs#{ConvId => Conv},
    SessConvs2 = SessConvs#{ConvId => SessConv},
    update_convs(Convs2, SessConvs2, State).


%% @private
update_active(ActiveId, #?NKOBJ{data=Data}=State) ->
    State#?NKOBJ{data = Data#?MODULE{active_id=ActiveId}}.


%% @private
update_convs(Convs, SessConvs, #?NKOBJ{data=Data}=State) ->
    State#?NKOBJ{
        data = Data#?MODULE{obj_convs=Convs, sess_convs=SessConvs},
        is_dirty = true
    }.


%% @private
do_event(Event, State) ->
    nkdomain_obj_util:event(Event, State).


%% @private
find_unread(Conv, #?NKOBJ{srv_id=SrvId}=State) ->
    #{obj_id:=ConvId, last_seen_message_time:=Time} = Conv,
    Search = #{
        filters => #{
            type => ?CHAT_MESSAGE,
            parent_id => ConvId,
            created_time => <<">", (integer_to_binary(Time))/binary>>
        },
        size => 0
    },
    case nkdomain_store:find(SrvId, Search) of
        {ok, Num, [], _Meta} ->
            Num;
        {error, Error} ->
            ?LLOG(error, "error reading unread count: ~p", [Error], State),
            0
    end.


%% @private
find_last_message(ConvId, #?NKOBJ{srv_id=SrvId}=State) ->
    Search = #{
        filters => #{
            type => ?CHAT_MESSAGE,
            parent_id => ConvId
        },
        fields => [created_time, created_by, ?CHAT_MESSAGE],
        sort => <<"desc:created_time">>,
        size => 1
    },
    case nkdomain_store:find(SrvId, Search) of
        {ok, 0, [], _Meta} ->
            #{};
        {ok, _Num, [Obj], _Meta} ->
            Obj;
        {error, Error} ->
            ?LLOG(error, "error reading unread count: ~p", [Error], State),
            #{}
    end.


