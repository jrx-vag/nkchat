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
-export([object_get_info/0, object_mapping/0, object_syntax/1,
         object_api_syntax/3, object_api_allow/4, object_api_cmd/4]).
-export([object_init/1, object_start/1, object_restore/1, object_send_event/2,
         object_sync_op/3, object_async_op/2, object_handle_info/2]).
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
%% TODO: if the session is stopped, an final event should be sent
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

-type sess_conv() :: #{linked=>boolean(), unread_count=>integer()}.

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
                last_seen_message_id => #{type => keyword},
                last_seen_message_time => #{type => date}
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
                    last_seen_message_id => binary,
                    last_seen_message_time => integer
                }}
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
    Data = #?MODULE{
        obj_convs = maps:from_list([{Id, C} || #{obj_id:=Id}=C <- ConvsList]),
        sess_convs = make_sess_convs(ConvsList, #{}, Session)
    },
    {ok, Session#obj_session{data=Data}}.


%% @private Prepare the object for saving
object_restore(#obj_session{obj = Obj, data = #?MODULE{} = Data} = Session) ->
%%    lager:error("SESS RESTORE: ~p", [lager:pr(Data, ?MODULE)]),
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
            case nkdomain_obj:is_enabled(ConvObjId) of
                {ok, true} ->
                    ?DEBUG("activated conversation ~s", [ConvObjId], Session),
                    Now = nklib_util:m_timestamp(),
                    Conv2 = Conv#{
                        last_active_time => Now,
                        last_seen_message_time => Now
                    },
                    SessConv2 = SessConv#{
                        unread_count => 0
                    },
                    Session2 = update_conv(ConvObjId, Conv2, SessConv2, Session),
                    #obj_session{data=Data} = Session2,
                    Session3 = Session2#obj_session{data=Data#?MODULE{active_id=ConvObjId}},
                    {ok, Reply, Session4} = get_conv_extra_info(ConvObjId, false, Session3),
                    Session5 = do_event({conversation_activated, ConvObjId}, Session4),
                    {reply_and_save, {ok, Reply}, Session5};
                {ok, false} ->
                    {reply, {error, object_is_disabled}, Session}
            end;
        not_found ->
            {reply, {error, conversation_not_found}, Session}
    end;

object_sync_op({?MODULE, add_conv, ConvId}, _From, Session) ->
    case get_conv(ConvId, Session) of
        not_found ->
            case link_conv(ConvId, Session) of
                {ok, ConvObjId} ->
                    % We don't want to count any older message as unread
                    Conv = #{
                        obj_id => ConvId,
                        last_active_time => 0,
                        last_seen_message_id => <<>>,
                        last_seen_message_time => nklib_util:m_timestamp()
                    },
                    SessConv = #{
                        linked => true,
                        unread_count => 0
                    },
                    ?DEBUG("added conversation ~s", [ConvObjId], Session),
                    Session2 = update_conv(ConvObjId, Conv, SessConv, Session),
                    Session3 = do_event({conversation_added, ConvObjId}, Session2),
                    {reply_and_save, {ok, ConvObjId}, Session3};
                {error, Error} ->
                    {reply, {error, Error}, Session}
            end;
        _ ->
            {reply, {error, conversation_is_already_member}, Session}
    end;

object_sync_op({?MODULE, rm_conv, ConvId}, _From, Session) ->
    case get_conv(ConvId, Session) of
        {ok, ConvObjId, _Conv, _SessConv} ->
            {Convs, SessConvs} = get_convs(Session),
            Convs2 = maps:remove(ConvObjId, Convs),
            SessConvs2 = maps:remove(ConvObjId, SessConvs),
            Session2 = update_convs(Convs2, SessConvs2, Session),
            ?DEBUG("removed conversation ~s", [ConvObjId], Session2),
            Session3 = do_event({conversation_removed, ConvId}, Session2),
            Reply = unlink_conv(ConvId, Session3),
            {reply_and_save, Reply, Session3};
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
    lager:error("Chat session online for ~s: ~p", [ConvId, Event]),
    nkdomain_obj:async_op(Pid, {?MODULE, conversation_event, ConvId, Event}).


%% @private
parse_offline_event(_SessId, ConvId, Event, UserId, Meta) ->
    lager:warning("Chat session offline for ~s: ~p (~s, ~p)", [ConvId, Event, UserId, Meta]).



%% @private
make_sess_convs([], Acc, _Session) ->
    Acc;

make_sess_convs([#{obj_id:=ConvId}|Rest], Acc, Session) ->
    Linked = case link_conv(ConvId, Session) of
        {ok, ConvId} ->
            true;
        {error, Error} ->
            ?LLOG(notice, "could not load conversation ~s: ~p", [ConvId, Error], Session),
            false
    end,
    make_sess_convs(Rest, Acc#{ConvId=>#{linked=>Linked, unread_count=>0}}, Session).


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
    {noreply, do_event({member_removed, ConvId, IsActive, MemberId}, Session)};

do_conversation_event({message_created, Msg}, ConvId, true, Conv, SessConv, Session) ->
    ?DEBUG("message event for active conversation", [], Session),
    #{obj_id:=MsgId, created_time:=Time} = Msg,
    Conv2 = Conv#{
        last_seen_message_id => MsgId,
        last_seen_message_time => Time
    },
    Session2 = update_conv(ConvId, Conv2, SessConv, Session),
    {noreply, do_event({message_created, Msg}, Session2)};

do_conversation_event({message_created, _Msg}, ConvId, false, Conv, SessConv, Session) ->
    ?DEBUG("message event for not active conversation", [], Session),
    Count = maps:get(unread_count, SessConv, 0) + 1,
    SessConv2 = SessConv#{unread_count=>Count},
    Session2 = update_conv(ConvId, Conv, SessConv2, Session),
    {noreply, do_event({unread_counter_updated, ConvId, Count}, Session2)};

do_conversation_event({message_updated, Msg}, _ConvId, true, _Conv, _SessConv, Session) ->
    {noreply, do_event({message_updated, Msg}, Session)};

do_conversation_event({message_deleted, MsgId}, _ConvId, true, _Conv, _SessConv, Session) ->
    {noreply, do_event({message_deleted, MsgId}, Session)};

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
            case nkchat_conversation_obj:get_sess_info(ConvObjId) of
                {ok, Data} when GetUnread->
                    {Num, _Time} = find_unread(Conv, Session),
                    SessConv2 = SessConv#{unread_count=>Num},
                    Data2 = Data#{unread_count => Num},
                    {ok, Data2, update_conv(ConvObjId, Conv, SessConv2, Session)};
                {ok, Data} ->
                    {ok, Data, Session};
                {error, _} ->
                    Data = Conv#{
                        is_enabled => false
                    },
                    {ok, Data, Session}
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
    #{obj_id:=ConvId, last_seen_message_id:=_MsgId, last_seen_message_time:=Time} = Conv,
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
            {Num, Time};
        {error, Error} ->
            ?LLOG(error, "error reading unread count: ~p", [Error], Session),
            {-1, Time}
    end.
