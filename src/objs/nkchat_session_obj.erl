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

%% @doc Session Object
-module(nkchat_session_obj).
-behavior(nkdomain_obj).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([create/2, find/2, start/3, stop/2, get_all_conversations/2, get_conversation/3]).
-export([set_active_conversation/3, add_conversation/3, remove_conversation/3]).
-export([conversation_event/6]).
-export([object_get_info/0, object_mapping/0, object_parse/3,
         object_api_syntax/3, object_api_allow/4, object_api_cmd/4]).
-export([object_init/1, object_start/1, object_restore/1, object_send_event/2,
         object_sync_op/3, object_async_op/2, object_handle_info/2]).
-export([object_admin_tree/4]).
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
%% If the caller stops, we will stop the session
%% TODO: if the session is stopped, a final event should be sent
start(Srv, Id, CallerPid) ->
    case nkdomain_obj_lib:load(Srv, Id, #{usage_link=>{CallerPid, ?MODULE}}) of
        #obj_id_ext{pid=Pid} ->
            nkdomain_obj:sync_op(Pid, {?MODULE, start, CallerPid});
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
    api_pids = [] :: [pid()],
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
object_api_syntax(Sub, Cmd, Syntax) ->
    nkchat_session_obj_syntax:api(Sub, Cmd, Syntax).


%% @private
object_api_allow(_Sub, _Cmd, _Data, State) ->
    {true, State}.


%% @private
object_api_cmd(Sub, Cmd, Req, State) ->
    nkchat_session_obj_api:cmd(Sub, Cmd, Req, State).


%% @private
object_send_event(Event, Session) ->
    nkchat_session_obj_events:event(Event, Session).


%% @private
%% We initialize soon in case of early terminate
object_init(Session) ->
    {ok, Session#obj_session{data=#?MODULE{}}}.


%% @private When the object is loaded, we make our cache
object_start(#obj_session{obj=Obj}=Session) ->
    #{?CHAT_SESSION := #{conversations := ConvsList}} = Obj,
    ObjConvs = maps:from_list([{Id, C} || #{obj_id:=Id}=C <- ConvsList]),
    SessConvs = lists:foldl(
        fun(ConvId, Acc) -> Acc#{ConvId => make_sess_conv(ConvId, Session)} end,
        #{},
        maps:keys(ObjConvs)),
    Data = #?MODULE{
        obj_convs = ObjConvs,
        sess_convs = SessConvs
    },
    {ok, Session#obj_session{data=Data}}.


%% @private Prepare the object for saving
object_restore(#obj_session{obj = Obj, data = #?MODULE{} = Data} = Session) ->
    #?MODULE{obj_convs = Convs} = Data,
    Obj2 = ?ADD_TO_OBJ(?CHAT_SESSION, #{conversations=>maps:values(Convs)}, Obj),
    {ok, Session#obj_session{obj = Obj2}}.


%% @private
object_sync_op({?MODULE, start, Pid}, From, #obj_session{data=Data}=Session) ->
    #?MODULE{api_pids=Pids} = Data,
    Data2 = Data#?MODULE{api_pids=[Pid|Pids]},
    Session2 = Session#obj_session{data=Data2},
    object_sync_op({?MODULE, get_all_conversations}, From, Session2);

object_sync_op({?MODULE, get_all_conversations}, _From, #obj_session{obj_id=ObjId}=Session) ->
    ConvIds = get_conv_ids(Session),
    {Reply, Session2} = get_convs_info(ConvIds, [], Session),
    {reply, {ok, ObjId, #{conversations=>Reply}}, Session2};

object_sync_op({?MODULE, get_conversation, ConvId}, _From, Session) ->
    case get_conv_extra_info(ConvId, true, Session) of
        {ok, Data, Session2} ->
            {reply, {ok, Data}, Session2};
        not_found ->
            {reply, {error, conversation_not_found}, Session}
    end;

object_sync_op({?MODULE, set_active_conv, ConvId}, _From, Session) ->
    case get_conv(ConvId, Session) of
        {ok, ConvObjId, Conv, SessConv} ->
            case do_set_active_conv(ConvObjId, Conv, SessConv, Session) of
                {ok, Reply, Session2} ->
                    {reply_and_save, {ok, Reply}, Session2};
                {error, Error} ->
                    {reply, {error, Error}, Session}
            end;
        not_found ->
            {reply, {error, conversation_not_found}, Session}
    end;

object_sync_op({?MODULE, add_conv, ConvId}, _From, Session) ->
    case get_conv(ConvId, Session) of
        not_found ->
            case do_add_conv(ConvId, Session) of
                {ok, ConvObjId, Session2} ->
                    {ok, Data, Session3} = get_conv_extra_info(ConvObjId, false, Session2),
                    {reply_and_save, {ok, Data}, Session3};
                {error, Error} ->
                    {reply, {error, Error}, Session}
            end;
        _ ->
            {reply, {error, conversation_is_already_member}, Session}
    end;

object_sync_op({?MODULE, rm_conv, ConvId}, _From, Session) ->
    case get_conv(ConvId, Session) of
        {ok, ConvObjId, Conv, SessConv} ->
            {ok, Reply, Session2} = do_rm_conv(ConvObjId, Conv, SessConv, Session),
            {reply_and_save, Reply, Session2};
        not_found ->
            {reply, {error, conversation_not_found}, Session}
    end;

object_sync_op(_Op, _From, _Session) ->
    continue.


%% @private
object_async_op({?MODULE, conversation_event, ConvId, Event}, Session) ->
    case get_conv(ConvId, Session) of
        {ok, ConvObjId, Conv, SessConv} ->
            IsActive = is_active(ConvId, Session),
            do_conversation_event(Event, ConvObjId, IsActive, Conv, SessConv, Session);
        not_found ->
            ?LLOG(notice, "received event for unknown conversation", [], Session)
    end;

object_async_op(_Op, _Session) ->
    continue.


object_handle_info(_Info, _Session) ->
    continue.


%% @doc
object_admin_tree(sessions, Num, Data, Acc) ->
    nkadmin_menu:add_tree_entry(menu_sessions_chat_sessions, {menuBadge, Num}, Data, Acc);

object_admin_tree(_Category, _Num, _Data, Acc) ->
    Acc.

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
do_set_active_conv(ConvId, Conv, SessConv, Session) ->
    case nkdomain_obj:is_enabled(ConvId) of
        {ok, true} ->
            ?DEBUG("activated conversation ~s", [ConvId], Session),
            {ok, Reply, Session2} = get_conv_extra_info(ConvId, false, Session),
            Now = nkdomain_util:timestamp(),
            Conv2 = Conv#{
                last_active_time => Now,
                last_seen_message_time => Now
            },
            SessConv2 = SessConv#{
                unread_count => 0
            },
            Session3 = update_conv(ConvId, Conv2, SessConv2, Session2),
            Session4 = update_active(ConvId, Session3),
            Session5 = do_event({conversation_activated, ConvId}, Session4),
            {ok, Reply, Session5};
        {ok, false} ->
            {error, object_is_disabled}
    end.


%% @private
do_add_conv(ConvId, Session) ->
    case link_conv(ConvId, Session) of
        {ok, ConvObjId} ->
            % We don't want to count any older message as unread
            Conv = #{
                obj_id => ConvId,
                last_active_time => 0,
                last_seen_message_time => nkdomain_util:timestamp()
            },
            SessConv = make_sess_conv(ConvId, Session),
            ?DEBUG("added conversation ~s", [ConvObjId], Session),
            Session2 = update_conv(ConvObjId, Conv, SessConv, Session),
            Session3 = do_event({conversation_added, ConvObjId}, Session2),
            {ok, ConvObjId, Session3};
        {error, Error} ->
            {error, Error}
    end.


%% @private
do_rm_conv(ConvId, _Conv, _SessConv, Session) ->
    {Convs, SessConvs} = get_convs(Session),
    Convs2 = maps:remove(ConvId, Convs),
    SessConvs2 = maps:remove(ConvId, SessConvs),
    Session2 = update_convs(Convs2, SessConvs2, Session),
    Session3 = case is_active(ConvId, Session) of
        true ->
            update_active(undefined, Session2);
        false ->
            Session2
    end,
    ?DEBUG("removed conversation ~s", [ConvId], Session2),
    Session4 = do_event({conversation_removed, ConvId}, Session3),
    Reply = unlink_conv(ConvId, Session4),
    {ok, Reply, Session4}.


%% @private generates a sess_conv object
make_sess_conv(ConvId, Session) ->
    Linked = case link_conv(ConvId, Session) of
        {ok, ConvId} ->
            true;
        {error, Error} ->
            ?LLOG(notice, "could not load conversation ~s: ~p", [ConvId, Error], Session),
            false
    end,
    LastMsg = find_last_message(ConvId, Session),
    #{
        linked => Linked,
        unread_count => 0,
        last_message => LastMsg,
        last_message_time => maps:get(<<"created_time">>, LastMsg, 0)
    }.



%% @private
link_conv(ConvId, #obj_session{srv_id=SrvId, obj_id=SessId, parent_id=UserId}) ->
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
unlink_conv(ConvId, #obj_session{obj_id=SessId, parent_id=UserId}) ->
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
do_conversation_event({member_added, MemberId}, ConvId, IsActive, _Conv, _SessConv, Session) ->
    {noreply, do_event({member_added, ConvId, IsActive, MemberId}, Session)};

do_conversation_event({member_removed, MemberId}, ConvId, IsActive, _Conv, _SessConv, Session) ->
    Session2 = do_event({member_removed, ConvId, IsActive, MemberId}, Session),
    case Session of
        #obj_session{parent_id=MemberId} ->
            case get_conv(ConvId, Session2) of
                {ok, ConvObjId, Conv, SessConv} ->
                    {ok, _Reply, Session3} = do_rm_conv(ConvObjId, Conv, SessConv, Session),
                    {noreply_and_save, Session3};
                not_found ->
                    {noreply, Session2}
            end;
        _ ->
            {noreply, Session2}
    end;

do_conversation_event({message_created, Msg}, ConvId, true, Conv, SessConv, Session) ->
    ?DEBUG("message event for active conversation", [], Session),
    #{created_time:=Time} = Msg,
    Conv2 = Conv#{
        last_seen_message_time => Time
    },
    SessConv2 = SessConv#{
        unread_count := 0,
        last_message := Msg,
        last_message_time := Time
    },
    Session2 = update_conv(ConvId, Conv2, SessConv2, Session),
    {noreply, do_event({message_created, ConvId, Msg}, Session2)};

do_conversation_event({message_created, Msg}, ConvId, false, Conv, SessConv, Session) ->
    ?DEBUG("message event for not active conversation", [], Session),
    #{created_time:=Time} = Msg,
    Count = maps:get(unread_count, SessConv, 0) + 1,
    SessConv2 = SessConv#{
        unread_count => Count,
        last_message => Msg,
        last_message_time => Time
    },
    Session2 = update_conv(ConvId, Conv, SessConv2, Session),
    {noreply, do_event({unread_counter_updated, ConvId, Count, Msg}, Session2)};

do_conversation_event({message_updated, Msg}, ConvId, true, _Conv, _SessConv, Session) ->
    {noreply, do_event({message_updated, ConvId, Msg}, Session)};

do_conversation_event({message_deleted, MsgId}, ConvId, true, _Conv, _SessConv, Session) ->
    {noreply, do_event({message_deleted, ConvId, MsgId}, Session)};

do_conversation_event(_Event, _ConvId, _IsActive, _Conv, _SessConv, Session) ->
    lager:notice("NOT PROCESSED EVENT: ~p", [_Event]),
    {noreply, Session}.



%% @private
get_convs_info([], Acc, Session) ->
    {Acc, Session};

get_convs_info([ConvId|Rest], Acc, Session) ->
    {ok, Data, Session2} = get_conv_info(ConvId, true, Session),
    get_convs_info(Rest, [Data|Acc], Session2).


%% @private
get_conv_info(ConvId, GetUnread, Session) ->
    case get_conv(ConvId, Session) of
        {ok, ConvObjId, Conv, SessConv} ->
            ConvData1 = maps:with([last_active_time, last_seen_message_time], Conv),
            #{unread_count:=Count0, last_message:=LastMsg, last_message_time:=LastMsgTime} = SessConv,
            Count = case GetUnread of
                true ->
                    find_unread(Conv, Session);
                false ->
                    Count0
            end,
            ConvData2 = ConvData1#{last_message => LastMsg, last_message_time => LastMsgTime, unread_count=>Count},
            Session2 = case Count of
                Count0 ->
                    Session;
                _ ->
                    update_conv(ConvObjId, Conv, SessConv#{unread_count:=Count}, Session)
            end,
            case nkchat_conversation_obj:get_sess_info(ConvObjId) of
                {ok, Data1} ->
                    Data2 = case Data1 of
                        #{subtype:=<<"one2one">>, member_ids:=MemberIds} ->
                            #obj_session{parent_id=UserId, srv_id=SrvId} = Session,
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
                    {ok, maps:merge(Data2, ConvData2), Session2};
                {error, Error} ->
                    ?LLOG(notice, "could not read conversation ~s: ~p", [ConvObjId, Error], Session2),
                    Data = ConvData2#{
                        is_enabled => false
                    },
                    {ok, Data, Session2}
            end;
        not_found ->
            not_found
    end.


%% @private
get_conv_extra_info(ConvId, GetUnread, #obj_session{srv_id=SrvId}=Session) ->
    case get_conv_info(ConvId, GetUnread, Session) of
        {ok, #{member_ids:=MemberIds}=Data, Session2} ->
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
            {ok, Data2#{members=>Members}, Session2};
        {ok, Data, Session} ->
            {ok, Data, Session};
        not_found ->
            not_found
    end.


%% @private
is_active(ConvId, #obj_session{data=#?MODULE{active_id=Active}}) ->
    ConvId == Active.


%% @private
get_conv(ConvId, Session) ->
    {Convs, SessConvs} = get_convs(Session),
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
get_conv_ids(#obj_session{data=#?MODULE{obj_convs=Convs}}) ->
    maps:keys(Convs).


%% @private
get_convs(#obj_session{data=#?MODULE{obj_convs=Convs, sess_convs=SessConvs}}) ->
    {Convs, SessConvs}.


%% @private
update_conv(ConvId, Conv, SessConv, #obj_session{data=Data}=Session) ->
    #?MODULE{obj_convs=Convs, sess_convs=SessConvs} = Data,
    Convs2 = Convs#{ConvId => Conv},
    SessConvs2 = SessConvs#{ConvId => SessConv},
    update_convs(Convs2, SessConvs2, Session).


%% @private
update_active(ActiveId, #obj_session{data=Data}=Session) ->
    Session#obj_session{data = Data#?MODULE{active_id=ActiveId}}.


%% @private
update_convs(Convs, SessConvs, #obj_session{data=Data}=Session) ->
    Session#obj_session{
        data = Data#?MODULE{obj_convs=Convs, sess_convs=SessConvs},
        is_dirty = true
    }.


%% @private
do_event(Event, Session) ->
    nkdomain_obj_util:event(Event, Session).


%% @private
find_unread(Conv, #obj_session{srv_id=SrvId}=Session) ->
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
            ?LLOG(error, "error reading unread count: ~p", [Error], Session),
            0
    end.


%% @private
find_last_message(ConvId, #obj_session{srv_id=SrvId}=Session) ->
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
            ?LLOG(error, "error reading unread count: ~p", [Error], Session),
            #{}
    end.


