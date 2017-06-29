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

-export([start/4, get_conversations/2]).
-export([set_active_conversation/3, add_conversation/3, remove_conversation/3]).
-export([conversation_event/4]).
-export([object_info/0, object_es_mapping/0, object_parse/3,
         object_api_syntax/2, object_api_cmd/2]).
-export([object_init/1, object_stop/2, object_send_event/2,
         object_sync_op/3, object_async_op/2]).
-export([object_admin_info/0]).
-export_type([meta/0, event/0]).

-include("nkchat.hrl").
-include_lib("nkdomain/include/nkdomain.hrl").
-include_lib("nkdomain/include/nkdomain_debug.hrl").
-include_lib("nkevent/include/nkevent.hrl").


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


-type start_opts() :: #{
    monitor => {module(), pid()},
    session_events => [binary()],
    session_id => term()
}.


%% ===================================================================
%% Public
%% ===================================================================


%% @doc Creates a new session
-spec start(nkservice:id(), nkdomain:id(), nkdomain:id(), start_opts()) ->
    {ok, nkdomain:obj_id(), pid()} | {error, term()}.

start(SrvId, DomainId, UserId, Opts) ->
    Obj = #{
        type => ?CHAT_SESSION,
        domain_id => DomainId,
        parent_id => UserId,
        created_by => UserId,
        active => true,
        ?CHAT_SESSION => #{}
    },
    Opts1 = maps:with([session_events, session_id], Opts),
    Opts2 = case Opts of
        #{monitor:=Monitor} ->
            Opts1#{meta=>#{monitor=>Monitor}};
        _ ->
            Opts1
    end,
    case nkdomain_obj_make:create(SrvId, Obj, Opts2) of
        {ok, #obj_id_ext{obj_id=SessId, pid=Pid}, _} ->
            {ok, SessId, Pid};
        {error, Error} ->
            {error, Error}
    end.


%% @doc
add_conversation(SrvId, Id, ConvId) ->
    case nkdomain_lib:load(SrvId, ConvId) of
        #obj_id_ext{type = ?CHAT_CONVERSATION, obj_id=ConvId2} ->
            nkdomain_obj:sync_op(SrvId, Id, {?MODULE, add_conv, ConvId2});
        {error, object_not_found} ->
            {error, conversation_not_found};
        {error, Error} ->
            {error, Error}
    end.


%% @doc
remove_conversation(SrvId, Id, ConvId) ->
    nkdomain_obj:sync_op(SrvId, Id, {?MODULE, rm_conv, ConvId}).


%% @doc
get_conversations(SrvId, Id) ->
    nkdomain_obj:sync_op(SrvId, Id, {?MODULE, get_conversations}).


%% @doc
-spec set_active_conversation(nkservice:id(), nkdomain:id(), nkdomain:id()) ->
    {ok, map()} | {error, term()}.

set_active_conversation(SrvId, Id, ConvId) ->
    nkdomain_obj:sync_op(SrvId, Id, {?MODULE, set_active_conv, ConvId}).


%% @doc Called from nkchat_conversation_obj
-spec conversation_event(nkservice:id(), nkdomain:obj_id(), meta(), term()) ->
    ok.

conversation_event(Pid, ConvId, _Meta, Event) ->
    lager:warning("NKLOG CONV EVENT ~p ~p", [Event, Pid]),
    nkdomain_obj:async_op(any, Pid, {?MODULE, conversation_event, ConvId, Event}).



%% ===================================================================
%% nkdomain_obj behaviour
%% ===================================================================

-type conv() :: #{
    name => binary(),
    description => binary(),
    type => binary(),
    last_active_time => nklib_util:m_timestamp(),
    member_ids => [binary()],
    pid => pid()
}.


-record(session, {
    user_id :: nkdomain:obj_id(),
    convs :: #{nkdomain:obj_id() => conv()},
    active_id :: undefined | nkdomain:obj_id(),
    api :: {module(), pid()}
}).



%% @private
object_info() ->
    #{
        type => ?CHAT_SESSION,
        stop_after_disabled => true,
        remove_after_stop => true
    }.


%% @doc
object_admin_info() ->
    #{
        class => session,
        weight => 2000,
        tree_id => <<"domain_tree_sessions_chat.sessions">>
    }.


%% @private
object_es_mapping() ->
    not_indexed.


%% @private
object_parse(_SrvId, _Mode, _Obj) ->
    #{}.


%% @private
object_api_syntax(Cmd, Syntax) ->
    nkchat_session_obj_syntax:api(Cmd, Syntax).


%% @private
object_api_cmd(Cmd, Req) ->
    nkchat_session_obj_api:cmd(Cmd, Req).


%% @private
object_send_event(Event, State) ->
    nkchat_session_obj_events:event(Event, State).


%% @private When the object is loaded, we make our cache
object_init(#?STATE{id=Id, obj=Obj, domain_id=DomainId, meta=Meta}=State) ->
    #obj_id_ext{srv_id=SrvId, obj_id=SessId} = Id,
    #{parent_id := UserId} = Obj,
    #{monitor:={ApiMod, ApiPid}} = Meta,
    Session = #session{
        user_id = UserId,
        convs = #{},
        api = {ApiMod, ApiPid}
    },
    State2 = State#?STATE{session=Session},
    {ok, Convs1} = nkchat_conversation_obj:find_member_conversations(SrvId, DomainId, UserId),
    State3 = lists:foldl(
        fun({ConvId, _Type}, Acc) ->
            case do_add_conv(ConvId, Acc) of
                {ok, Acc2} ->
                    Acc2;
                {error, Error} ->
                    ?LLOG(warning, "could not load conversation ~s: ~p", [ConvId, Error], State),
                    Acc
            end
        end,
        State2,
        Convs1),
    ok = nkdomain_user_obj:register_session(SrvId, UserId, ?CHAT_SESSION, SessId, #{}),
    State4 = nkdomain_obj_util:link_to_api_server(?MODULE, ApiMod, ApiPid, State3),
    {ok, State4}.


%% @private
object_stop(_Reason, State) ->
    {ok, nkdomain_obj_util:unlink_from_api_server(?MODULE, State)}.


%%object_sync_op({?MODULE, get_all_conversations}, _From, #?STATE{id=#obj_id_ext{obj_id=ObjId}}=State) ->
%%    ConvIds = get_conv_ids(State),
%%    {Reply, State2} = get_convs_info(ConvIds, [], State),
%%    {reply, {ok, ObjId, #{conversations=>Reply}}, State2};
%%
%%object_sync_op({?MODULE, get_conversation, ConvId}, _From, State) ->
%%    case get_conv_extra_info(ConvId, true, State) of
%%        {ok, Data, State2} ->
%%            {reply, {ok, Data}, State2};
%%        not_found ->
%%            {reply, {error, conversation_not_found}, State}
%%    end;

object_sync_op({?MODULE, set_active_conv, ConvId}, _From, State) ->
    case get_conv(ConvId, State) of
        {ok, _} ->
            {reply, ok, do_set_active_conv(ConvId, State)};
        not_found ->
            {reply, {error, conversation_not_found}, State}
    end;

object_sync_op({?MODULE, add_conv, ConvId}, _From, State) ->
    case get_conv(ConvId, State) of
        not_found ->
            case do_add_conv(ConvId, State) of
                {ok, State2} ->
                    ?DEBUG("added conversation ~s", [ConvId], State2),
                    State3 = do_event({conversation_added, ConvId}, State2),
                    {reply, ok, State3};
                {error, Error} ->
                    {reply, {error, Error}, State}
            end;
        {ok, _} ->
            {reply, {error, conversation_is_already_present}, State}
    end;

object_sync_op({?MODULE, rm_conv, ConvId}, _From, State) ->
    case do_rm_conv(ConvId, State) of
        {ok, State2} ->
            ?DEBUG("removed conversation ~s", [ConvId], State2),
            State3 = do_event({conversation_removed, ConvId}, State2),
            {reply, ok, State3};
        {error, Error} ->
            {reply, {error, Error}, State}
    end;

object_sync_op(_Op, _From, _State) ->
    continue.


%% @private
object_async_op({?MODULE, conversation_event, ConvId, Event}, State) ->
    case get_conv(ConvId, State) of
        {ok, _} ->
            do_conversation_event(Event, ConvId, State);
        not_found ->
            ?LLOG(notice, "received event for unknown conversation", [], State)
    end;

object_async_op(_Op, _State) ->
    continue.



%% ===================================================================
%% Internal
%% ===================================================================


%% @private
do_set_active_conv(ConvId, #?STATE{session=Session, id=#obj_id_ext{obj_id=SessId}}=State) ->
    #session{user_id=UserId, convs=Convs} = Session,
    do_set_active_conv(maps:to_list(Convs), ConvId, UserId, SessId),
    Session2 = Session#session{active_id=ConvId},
    State#?STATE{session=Session2}.


%% @private
do_set_active_conv([], _ActiveId, _UserId, _SessId) ->
    ok;

do_set_active_conv([{ConvId, #{pid:=Pid}}|Rest], ActiveId, UserId, SessId) ->
    Active = ActiveId == ConvId,
    nkchat_conversation_obj:set_session_active(any, Pid, UserId, SessId, Active),
    do_set_active_conv(Rest, ActiveId, UserId, SessId).


%% @private
do_add_conv(ConvId, State) ->
    #?STATE{srv_id=SrvId, id=#obj_id_ext{obj_id=SessId}} = State,
    #?STATE{session=Session} = State,
    #session{user_id=UserId, convs=Convs1} = Session,
    case nkchat_conversation_obj:add_session(SrvId, ConvId, UserId, SessId, #{}) of
        {ok, ConvData, Pid} ->
            monitor(process, Pid),
            Convs2 = Convs1#{ConvId => ConvData#{pid=>Pid}},
            Session2 = Session#session{convs=Convs2},
            {ok, State#?STATE{session=Session2}};
        {error, Error} ->
            {error, Error}
    end.


%% @private
do_rm_conv(ConvId, State) ->
    case get_conv(ConvId, State) of
        {ok, #{pid:=Pid}} ->
            #?STATE{id=#obj_id_ext{obj_id=SessId}} = State,
            #?STATE{session=Session} = State,
            #session{user_id=UserId, convs=Convs1, active_id=ActiveId} = Session,
            nkchat_conversation_obj:remove_session(any, Pid, UserId, SessId),
            Convs2 = maps:remove(ConvId, Convs1),
            Session2 = Session#session{convs=Convs2},
            Session3 = case ActiveId of
                ConvId ->
                    Session2#session{active_id=undefined};
                _ ->
                    Session2
            end,
            {ok, State#?STATE{session=Session3}};
        not_found ->
            {error, conversation_not_found}
    end.


%% @private
do_conversation_event({member_added, MemberId}, ConvId, State) ->
    {noreply, do_event({member_added, ConvId, MemberId}, State)};

do_conversation_event({member_removed, MemberId}, ConvId, State) ->
    {noreply, do_event({member_removed, ConvId, MemberId}, State)};

do_conversation_event({message_created, Msg}, ConvId, State) ->
    {noreply, do_event({message_created, ConvId, Msg}, State)};

do_conversation_event({message_updated, Msg}, ConvId, State) ->
    {noreply, do_event({message_updated, ConvId, Msg}, State)};

do_conversation_event({message_deleted, MsgId}, ConvId, State) ->
    {noreply, do_event({message_deleted, ConvId, MsgId}, State)};

do_conversation_event({counter_updated, Counter}, ConvId, State) ->
    {noreply, do_event({unread_counter_updated, ConvId, Counter}, State)};

do_conversation_event(_Event, _ConvId, State) ->
    {noreply, State}.



%%%% @private
%%get_convs_info([], Acc, State) ->
%%    {Acc, State};
%%
%%get_convs_info([ConvId|Rest], Acc, State) ->
%%    {ok, Data, State2} = get_conv_info(ConvId, true, State),
%%    get_convs_info(Rest, [Data|Acc], State2).
%%
%%
%%%% @private
%%get_conv_info(ConvId, GetUnread, #?STATE{srv_id=SrvId}=State) ->
%%    case get_conv(ConvId, State) of
%%        {ok, ConvObjId, Conv, SessConv} ->
%%            ConvData1 = maps:with([last_active_time, last_seen_message_time], Conv),
%%            #{unread_count:=Count0, last_message:=LastMsg, last_message_time:=LastMsgTime} = SessConv,
%%            Count = case GetUnread of
%%                true ->
%%                    find_unread(Conv, State);
%%                false ->
%%                    Count0
%%            end,
%%            ConvData2 = ConvData1#{last_message => LastMsg, last_message_time => LastMsgTime, unread_count=>Count},
%%            State2 = case Count of
%%                Count0 ->
%%                    State;
%%                _ ->
%%                    update_conv(ConvObjId, Conv, SessConv#{unread_count:=Count}, State)
%%            end,
%%            case nkchat_conversation_obj:get_sess_info(SrvId, ConvObjId) of
%%                {ok, Data1} ->
%%                    Data2 = case Data1 of
%%                        #{type:=one2one, member_ids:=MemberIds} ->
%%                            #?STATE{domain_id=UserId, srv_id=SrvId} = State,
%%                            case MemberIds -- [UserId] of
%%                                [PeerId] ->
%%                                    case nkdomain_user_obj:get_name(SrvId, PeerId) of
%%                                        {ok, User} ->
%%                                            maps:remove(member_ids, Data1#{peer=>User});
%%                                        {error, _Error} ->
%%                                            Data1
%%                                    end;
%%                                _ ->
%%                                    Data1
%%                            end;
%%                        _ ->
%%                            Data1
%%                    end,
%%                    {ok, maps:merge(Data2, ConvData2), State2};
%%                {error, Error} ->
%%                    ?LLOG(notice, "could not read conversation ~s: ~p", [ConvObjId, Error], State2),
%%                    Data = ConvData2#{
%%                        is_enabled => false
%%                    },
%%                    {ok, Data, State2}
%%            end;
%%        not_found ->
%%            not_found
%%    end.
%%
%%
%%%% @private
%%get_conv_extra_info(ConvId, GetUnread, #?STATE{srv_id=SrvId}=State) ->
%%    case get_conv_info(ConvId, GetUnread, State) of
%%        {ok, #{member_ids:=MemberIds}=Data, State2} ->
%%            Data2 = maps:remove(member_ids, Data),
%%            Members = lists:foldl(
%%                fun(UserId, Acc) ->
%%                    case nkdomain_user_obj:get_name(SrvId, UserId) of
%%                        {ok, User} -> [User|Acc];
%%                        {error, _Error} -> Acc
%%                    end
%%                end,
%%                [],
%%                MemberIds),
%%            {ok, Data2#{members=>Members}, State2};
%%        {ok, Data, State} ->
%%            {ok, Data, State};
%%        not_found ->
%%            not_found
%%    end.


%% @private
get_conv(ConvId, #?STATE{session=Session}) ->
    #session{convs=Convs} = Session,
    case maps:find(ConvId, Convs) of
        {ok, Conv} ->
            {ok, Conv};
        error ->
            not_found
    end.

%% @private
do_event(Event, State) ->
    nkdomain_obj_util:event(Event, State).
