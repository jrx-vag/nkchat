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
-export([conversation_event/2]).
-export([object_get_info/0, object_mapping/0, object_syntax/1,
         object_api_syntax/3, object_api_allow/4, object_api_cmd/4]).
-export([object_start/1, object_stop/2, object_save/1,
         object_sync_op/3, object_async_op/2, object_handle_info/2]).
-export([find_unread/2]).
-export([send_api_event/2]).        % Exported for mocking in tests

-include("nkchat.hrl").
-include_lib("nkdomain/include/nkdomain.hrl").
-include_lib("nkdomain/include/nkdomain_debug.hrl").
-include_lib("nkservice/include/nkservice.hrl").


%% Period to find for inactive conversations
-define(CHECK_TIME, 5*60*1000).


%% ===================================================================
%% Types
%% ===================================================================


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
conversation_event(Pid, #event{class = ?CHAT_CONVERSATION}=Event) ->
    ok = nkdomain_obj:async_op(Pid, {?MODULE, conversation_event, Event}).



%% ===================================================================
%% nkdomain_obj behaviour
%% ===================================================================

-type sess_conv() :: #{linked=>boolean(), unread_count=>integer()}.

-record(?MODULE, {
    convs :: #{nkdomain:obj_id() => map()},
    sess_convs :: #{nkdomain:obj_id() => sess_conv()},
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
                last_delivered_message_id => #{type => keyword},
                last_delivered_message_time => #{type => date}
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
                    last_delivered_message_id => binary,
                    last_delivered_message_time => integer,
                    '__mandatory' => [
                        <<?CHAT_SESSION/binary, ".conversations.obj_id">>,
                        <<?CHAT_SESSION/binary, ".conversations.last_active_time">>,
                        <<?CHAT_SESSION/binary, ".conversations.last_delivered_message_id">>,
                        <<?CHAT_SESSION/binary, ".conversations.last_delivered_message_time">>
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
object_start(#obj_session{obj=Obj}=Session) ->
    #{?CHAT_SESSION := #{conversations := ConvsList}} = Obj,
    Data = #?MODULE{
        convs = maps:from_list([{Id, C} || #{obj_id:=Id}=C <- ConvsList]),
        sess_convs = make_sess_convs(ConvsList, #{}, Session)
    },
    self() ! {?MODULE, check_time},
    {ok, Session#obj_session{data=Data}}.


%% @private Prepare the object for saving
object_save(Session) ->
    {ok, update_obj(Session)}.


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
    case get_conv_extra_info(ConvId, Session) of
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
                    Conv2 = Conv#{
                        last_active_time => nklib_util:m_timestamp()
                    },
                    SessConv2 = SessConv#{
                        unread_count => 0
                    },
                    Session2 = update_conv(ConvObjId, Conv2, SessConv2, Session),
                    #obj_session{data=Data} = Session2,
                    Session3 = Session2#obj_session{data=Data#?MODULE{active_id=ConvObjId}},
                    {ok, Reply, Session4} = get_conv_extra_info(ConvObjId, Session3),
                    {reply_and_save, {ok, Reply}, Session4};
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
                        last_delivered_message_id => <<>>,
                        last_delivered_message_time => nklib_util:m_timestamp()
                    },
                    SessConv = #{
                        linked => true,
                        unread_count => 0
                    },
                    ?DEBUG("added conversation ~s", [ConvObjId], Session),
                    Session2 = update_conv(ConvObjId, Conv, SessConv, Session),
                    Body = #{conversation => Conv#{'_enabled'=>true}},
                    Event = session_event(<<>>, <<"conversation_added">>, Body, Session2),
                    ?MODULE:send_api_event(Event, Session2),
                    {reply_and_save, {ok, ConvObjId}, Session2};
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
            Body = #{conversation => #{obj_id=>ConvObjId}},
            Event = session_event(<<>>, <<"conversation_removed">>, Body, Session2),
            ?MODULE:send_api_event(Event, Session2),
            {reply_and_save, ok, Session2};
        not_found ->
            {reply, {error, conversation_not_found}, Session}
    end;

object_sync_op(_Op, _From, _Session) ->
    continue.


%% @private
object_async_op({?MODULE, conversation_event,
                #event{subclass = <<"message">>, type = <<"created">>}=Event}, Session) ->
    #event{obj_id=ConvId, body = #{message_id:=MsgId, created_time:=Time}=Body} = Event,
    Active = get_active(Session),
    case get_conv(ConvId, Session) of
        {ok, ConvId, Conv, SessConv} when Active==ConvId ->
            Body2 = Body#{conversation_id=>ConvId},
            Event2 = session_event(?CHAT_CONVERSATION, <<"message_created">>, Body2, Session),
            ?MODULE:send_api_event(Event2, Session),
            Conv2 = Conv#{
                last_delivered_message_id => MsgId,
                last_delivered_message_time => Time
            },
            Session2 = update_conv(ConvId, Conv2, SessConv, Session),
            ?DEBUG("set last message for conversation ~s", [ConvId], Session),
            {noreply, Session2};
        {ok, ConvId, Conv, SessConv} ->
            Count = maps:get(unread_count, SessConv, 0) + 1,
            SessConv2 = SessConv#{unread_count=>Count},
            Session2 = update_conv(ConvId, Conv, SessConv2, Session),
            Body2 = #{conversation_id=>ConvId, counter=>Count},
            Event2 = session_event(?CHAT_CONVERSATION, <<"unread_counter">>, Body2, Session),
            ?MODULE:send_api_event(Event2, Session2),
            {noreply, Session2};
        not_found ->
            ?LLOG(warning, "received event for unknown conversation", [], Session),
            {noreply, Session}
    end;

object_async_op({?MODULE, conversation_event, #event{subclass = <<"message">>, type=Type}=Event}, Session)
        when Type == <<"updated">>; Type == <<"deleted">> ->
    #event{type=Type, obj_id=ConvId, body=Body} = Event,
    case get_active(Session) of
        ConvId ->
            Type2 = case Type of
                <<"updated">> -> <<"message_updated">>;
                <<"deleted">> -> <<"message_deleted">>
            end,
            Body2 = Body#{conversation_id=>ConvId},
            Event2 = session_event(?CHAT_CONVERSATION, Type2, Body2, Session),
            ?MODULE:send_api_event(Event2, Session);
        _ ->
            ok
    end,
    {noreply, Session};

object_async_op({?MODULE, conversation_event, #event{subclass = <<"member">>, type=Type}=Event}, Session)
        when Type == <<"added">>; Type == <<"removed">> ->
    #event{type=Type, obj_id=ConvId, body=Body} = Event,
    case get_active(Session) of
        ConvId ->
            Type2 = case Type of
                <<"added">> -> <<"member_added">>;
                <<"removed">> -> <<"member_removed">>
            end,
            Body2 = Body#{conversation_id=>ConvId},
            Event2 = session_event(?CHAT_CONVERSATION, Type2, Body2, Session),
            ?MODULE:send_api_event(Event2, Session);
        _ ->
            ok
    end,
    {noreply, Session};

object_async_op({?MODULE, conversation_event, Event}, Session) ->
    ?LLOG(warning, "unexpected conversation event: ~p", [Event], Session),
    {noreply, Session};

object_async_op(_Op, _Session) ->
    continue.


%%%% @private
%%object_handle_info({?MODULE, check_time}, Session) ->
%%    Monitor = get_monitor(Session),
%%    Session2 = case nkdomain_monitor:reload_disabled(Monitor) of
%%        {[], _} ->
%%            Session;
%%        {ObjIds, Monitor2} ->
%%            ?LLOG(notice, "reloaded objects ~p", [ObjIds], Session),
%%            update_monitor(Monitor2, Session)
%%    end,
%%    erlang:send_after(?CHECK_TIME, self(), {?MODULE, check_time}),
%%    {noreply, Session2};
%%
%%object_handle_info({'DOWN', _Ref, process, Pid, _Reason}, Session) ->
%%    Monitor1 = get_monitor(Session),
%%    case nkdomain_monitor:down_obj(Pid, Monitor1) of
%%        {ok, ObjId, _Obj, Monitor2} ->
%%            ?LLOG(notice, "member ~s is disabled", [ObjId], Session),
%%            {noreply, update_monitor(Monitor2, Session)};
%%        not_found ->
%%            continue
%%    end;

object_handle_info(_Info, _Session) ->
    continue.


%% @private
object_stop(_Reason, Session) ->
    Event = session_event(<<>>, <<"session_stopped">>, #{}, Session),
    ?MODULE:send_api_event(Event, Session),
    {ok, Session}.





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
        usage_link => {SessId, {?MODULE, usage, SessId}},
        event_link => {SessId, {?MODULE, event, SessId}}
    },
    case nkdomain_obj_lib:load(SrvId, ConvId, Opts) of
        #obj_id_ext{obj_id=ConvObjId, pid=Pid} ->
            ok = nkchat_conversation_obj:register_session(Pid, UserId, {?MODULE, self()}),
            {ok, ConvObjId};
        {error, Error} ->
            {error, Error}
    end.


%% @private
get_convs_info([], Acc, Session) ->
    {Acc, Session};

get_convs_info([ConvId|Rest], Acc, Session) ->
    {ok, Data, Session2} = get_conv_info(ConvId, Session),
    get_convs_info(Rest, [Data|Acc], Session2).


%% @private
get_conv_info(ConvId, Session) ->
    case get_conv(ConvId, Session) of
        {ok, ConvObjId, Conv, SessConv} ->
            case nkdomain_obj:get_session(ConvId) of
                {ok, #obj_session{is_enabled=Enabled, path=Path, obj=ConvObj}} ->
                    #{
                        description := Description,
                        ?CHAT_CONVERSATION := #{member_ids:=MemberIds}
                    } = ConvObj,
                    {Num, _Time} = find_unread(Conv, Session),
                    SessConv2 = SessConv#{unread_count=>Num},
                    Data = Conv#{
                        path => Path,
                        description => Description,
                        is_enabled => Enabled,
                        member_ids => MemberIds,
                        unread_count => Num
                    },
                    {ok, Data, update_conv(ConvObjId, Conv, SessConv2, Session)};
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
get_conv_extra_info(ConvId, #obj_session{srv_id=SrvId}=Session) ->
    case get_conv_info(ConvId, Session) of
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
get_active(#obj_session{data=#?MODULE{active_id=Active}}) ->
    Active.


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
get_conv_ids(#obj_session{data=#?MODULE{convs=Convs}}) ->
    maps:keys(Convs).


%% @private
get_convs(#obj_session{data=#?MODULE{convs=Convs, sess_convs=SessConvs}}) ->
    {Convs, SessConvs}.


%% @private
update_conv(ConvId, Conv, SessConv, #obj_session{data=Data}=Session) ->
    #?MODULE{convs=Convs, sess_convs=SessConvs} = Data,
    Convs2 = Convs#{ConvId => Conv},
    SessConvs2 = SessConvs#{ConvId => SessConv},
    update_convs(Convs2, SessConvs2, Session).


%% @private
update_convs(Convs, SessConvs, #obj_session{data=Data}=Session) ->
    Session#obj_session{
        data = Data#?MODULE{convs=Convs, sess_convs=SessConvs},
        is_dirty = true
    }.


%% @private
update_obj(#obj_session{obj=Obj, data=Data}=Session) ->
    #?MODULE{convs=Convs} = Data,
    Obj2 = ?ADD_TO_OBJ(?CHAT_SESSION, #{conversations=>maps:values(Convs)}, Obj),
    Session#obj_session{obj=Obj2}.


%% @private
find_unread(Conv, #obj_session{srv_id=SrvId}=Session) ->
    #{obj_id:=ConvId, last_delivered_message_id:=_MsgId, last_delivered_message_time:=Time} = Conv,
    Search = #{
        filters => #{
            type => ?CHAT_MESSAGE,
            parent_id => ConvId,
            created_time => {Time+1, none}
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


%% @private
send_api_event(Event, #obj_session{data=Data}) ->
    case Data of
        #?MODULE{api_pids=Pids} ->
            lists:foreach(fun(Pid) -> nkapi_server:event(Pid, Event) end, Pids);
        _ ->
            ok
    end.



%% @private
session_event(Sub, Type, Body, #obj_session{srv_id=SrvId, obj_id=ObjId}) ->
    #event{
        srv_id = SrvId,
        class = ?CHAT_SESSION,
        subclass = Sub,
        type = Type,
        obj_id = ObjId,
        body = Body
    }.
