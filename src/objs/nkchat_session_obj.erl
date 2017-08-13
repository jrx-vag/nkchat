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

-export([start/4, get_conversations/2, get_conversation_info/3]).
-export([set_active_conversation/3, add_conversation/3, remove_conversation/3]).
-export([conversation_event/4, send_invitation/5, accept_invitation/3, reject_invitation/3]).
-export([object_info/0, object_es_mapping/0, object_parse/3,
         object_api_syntax/2, object_api_cmd/2]).
-export([object_init/1, object_stop/2, object_send_event/2,
         object_sync_op/3, object_async_op/2]).
-export([object_admin_info/0]).
-export([notify_fun/5]).

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
    {unread_counter_updated, ConvId::nkdomain:obj_id(), integer()} |
    {invited_to_conversation, TokenId::binary(), UserId::binary(), ConvId::binary()} |
    {remove_notification, TokenId::binary()}.


-type start_opts() :: #{
    session_link => {module(), pid()},
    session_events => [binary()]
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
    Opts2 = maps:with([session_link, session_events], Opts),
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
    case nkdomain_lib:find(SrvId, ConvId) of
        {ok, _Type, ConvId2, _Pid} ->
            nkdomain_obj:sync_op(SrvId, Id, {?MODULE, rm_conv, ConvId2});
        {error, Error} ->
            {error, Error}
    end.


%% @doc
-spec get_conversations(nkservice:id(), nkdomain:id()) ->
    {ok, [nkdomain:obj_id()]} | {error, term()}.

get_conversations(SrvId, Id) ->
    nkdomain_obj:sync_op(SrvId, Id, {?MODULE, get_conversations}).


%% @doc
-spec get_conversation_info(nkservice:id(), nkdomain:id(), nkdomain:id()) ->
    {ok, map()} | {error, term()}.

get_conversation_info(SrvId, Id, Conv) ->
    case nkdomain_lib:find(SrvId, Conv) of
        {ok, _Type, ConvId, _Pid} ->
            nkdomain_obj:sync_op(SrvId, Id, {?MODULE, get_conversation_info, ConvId});
        {error, Error} ->
            {error, Error}
    end.


%% @doc
-spec set_active_conversation(nkservice:id(), nkdomain:id(), nkdomain:id()) ->
    {ok, map()} | {error, term()}.

set_active_conversation(SrvId, Id, Conv) ->
    case nkdomain_lib:find(SrvId, Conv) of
        {ok, _Type, ConvId, _Pid} ->
            nkdomain_obj:sync_op(SrvId, Id, {?MODULE, set_active_conv, ConvId});
        {error, Error} ->
            {error, Error}
    end.

%% @doc Sends a invitation notification
-spec send_invitation(nkservice:id(), nkdomain:id(), nkdomain:id(), nkdomain:id(), integer()) ->
    {ok, TokenId::nkdomain:obj_id()} | {error, term()}.

send_invitation(SrvId, SessId, MemberId, ConvId, TTL) ->
    nkdomain_obj:sync_op(SrvId, SessId, {?MODULE, send_invitation, MemberId, ConvId, TTL}).


%% @doc Accepts a invitation notification
-spec accept_invitation(nkservice:id(), nkdomain:id(), nkdomain:id()) ->
    ok | {error, term()}.

accept_invitation(SrvId, SessId, TokenId) ->
    case nkdomain_token_obj:consume_token(SrvId, TokenId, accepted) of
        {ok, #{data:=Data}} ->
            nkdomain_obj:sync_op(SrvId, SessId, {?MODULE, accept_invitation, Data});
        {error, Error} ->
            {error, Error}
    end.


%% @doc Rejects a invitation notification
-spec reject_invitation(nkservice:id(), nkdomain:id(), nkdomain:id()) ->
    ok | {error, term()}.

reject_invitation(SrvId, _SessId, TokenId) ->
    case nkdomain_token_obj:consume_token(SrvId, TokenId, rejected) of
        {ok, _Data} ->
            lager:error("NKLOG Token consumed"),
            ok;
        {error, Error} ->
            {error, Error}
    end.


%% @doc Called from nkchat_conversation_obj
-spec conversation_event(nkservice:id(), nkdomain:obj_id(), meta(), term()) ->
    ok.

conversation_event(Pid, ConvId, _Meta, Event) ->
    lager:warning("NKLOG CONV EVENT ~p ~p", [Event, Pid]),
    nkdomain_obj:async_op(any, Pid, {?MODULE, conversation_event, ConvId, Event}).


%% @private To be called from nkdomain_user_obj
notify_fun(_SessId, Pid, TokenId, Msg, Op) ->
    % lager:error("NKLOG SESS FUN ~p ~p", [Op, Msg]),
    nkdomain_obj:async_op(any, Pid, {?MODULE, notify, TokenId, Msg, Op}).



%% ===================================================================
%% nkdomain_obj behaviour
%% ===================================================================

-record(session, {
    user_id :: nkdomain:obj_id(),
    conv_pids :: #{nkdomain:obj_id() => {Data::map(), pid()}},
    active_id :: undefined | nkdomain:obj_id()
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
    #{
        vsn => #{type => keyword}
    }.


%% @private
object_parse(_SrvId, _Mode, _Obj) ->
    #{
        vsn => binary,
        '__defaults' => #{vsn => <<"1">>}
    }.


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
object_init(#?STATE{id=Id, obj=Obj, domain_id=DomainId}=State) ->
    #obj_id_ext{srv_id=SrvId, obj_id=SessId} = Id,
    #{parent_id := UserId} = Obj,
    Session = #session{
        user_id = UserId,
        conv_pids = #{}
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
    Opts = #{notify_fun => fun ?MODULE:notify_fun/5},
    ok = nkdomain_user_obj:register_session(SrvId, UserId, DomainId, ?CHAT_SESSION, SessId, Opts),
    State4 = nkdomain_obj_util:link_to_session_server(?MODULE, State3),
    {ok, State4}.


%% @private
object_stop(_Reason, State) ->
    {ok, nkdomain_obj_util:unlink_from_session_server(?MODULE, State)}.


%% @private
object_sync_op({?MODULE, get_conversations}, _From, #?STATE{session=Session}=State) ->
    #session{conv_pids=Convs} = Session,
    {reply, {ok, maps:keys(Convs)}, State};

object_sync_op({?MODULE, get_conversation_info, ConvId}, _From, #?STATE{session=Session}=State) ->
    #session{user_id=UserId} = Session,
    case get_conv_pid(ConvId, State) of
        {ok, Pid} ->
            {ok, Info} = nkchat_conversation_obj:get_member_info(any, Pid, UserId),
            {reply, {ok, Info}, State};
        not_found ->
            {reply, {error, conversation_not_found}, State}
    end;

object_sync_op({?MODULE, set_active_conv, ConvId}, _From, State) ->
    case get_conv_pid(ConvId, State) of
        {ok, _} ->
            {reply, ok, do_set_active_conv(ConvId, State)};
        not_found ->
            {reply, {error, conversation_not_found}, State}
    end;

object_sync_op({?MODULE, add_conv, ConvId}, _From, State) ->
    case get_conv_pid(ConvId, State) of
        not_found ->
            case do_add_conv(ConvId, State) of
                {ok, State2} ->
                    ?DEBUG("added conversation ~s", [ConvId], State2),
                    State3 = do_event({conversation_added, ConvId}, State2),
                    {reply, {ok, ConvId}, State3};
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
            {reply, ok, State2};
        {error, Error} ->
            {reply, {error, Error}, State}
    end;

object_sync_op({?MODULE, send_invitation, Member, Conv, TTL}, _From, State) ->
    #?STATE{srv_id=SrvId, domain_id=DomainId, parent_id=UserId, id=#obj_id_ext{obj_id=SessId}} = State,
    Reply = case nkchat_conversation_obj:add_invite_op(SrvId, Conv, UserId, Member, #{}) of
        {ok, ConvId, MemberId, UserId, Op1} ->
            case nkdomain_user_obj:add_notification_op(SrvId, MemberId, ?CHAT_SESSION, #{}, Op1) of
                {ok, MemberId, Op2} ->
                    Op3 = Op2#{
                        ?CHAT_SESSION => #{
                            <<"invited_to_conversation_op">> => #{
                                <<"session_id">> => SessId,
                                <<"conversation_id">> => ConvId,
                                <<"user_id">> => UserId,
                                <<"member_id">> => MemberId
                            }
                        }
                    },
                    Opts = #{ttl => TTL},
                    case nkdomain_token_obj:create(SrvId, DomainId, MemberId, UserId, <<"chat.invite">>, Opts, Op3) of
                        {ok, TokenId, _Pid, _Secs, _Unknown} ->
                            {ok, TokenId};
                        {error, Error} ->
                            {error, Error}
                    end;
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end,
    {reply, Reply, State};

object_sync_op({?MODULE, accept_invitation, Data}, _From, #?STATE{srv_id=SrvId}=State) ->
    Reply = nkchat_conversation_obj:perform_op(SrvId, Data),
    {reply, Reply, State};

object_sync_op(_Op, _From, _State) ->
    continue.


%% @private
object_async_op({?MODULE, conversation_event, ConvId, Event}, State) ->
    case get_conv_pid(ConvId, State) of
        {ok, _} ->
            do_conversation_event(Event, ConvId, State);
        not_found ->
            ?LLOG(notice, "received event ~p for unknown conversation", [Event], State),
            {noreply, State}
    end;

object_async_op({?MODULE, notify, TokenId, Msg, Op}, State) ->
    case Msg of
        #{
            ?CHAT_SESSION := #{
                <<"invited_to_conversation_op">> := #{
                    <<"conversation_id">> := ConvId,
                    <<"user_id">> := UserId,
                    <<"member_id">> := _MemberId
                }
            }
        } ->
            Event = case Op of
                created ->
                    {invited_to_conversation, TokenId, UserId, ConvId};
                removed ->
                    {remove_notification, TokenId}
            end,
            {noreply, do_event(Event, State)};
        _ ->
            ?LLOG(warning, "unxepected notify: ~p", [Msg], State),
            {noreply, State}
    end;

object_async_op(_Op, _State) ->
    continue.



%% ===================================================================
%% Internal
%% ===================================================================


%% @private
do_set_active_conv(ConvId, #?STATE{session=Session, id=#obj_id_ext{obj_id=SessId}}=State) ->
    #session{user_id=UserId, conv_pids=ConvPids} = Session,
    do_set_active_conv(maps:to_list(ConvPids), ConvId, UserId, SessId),
    Session2 = Session#session{active_id=ConvId},
    State#?STATE{session=Session2}.


%% @private
do_set_active_conv([], _ActiveId, _UserId, _SessId) ->
    ok;

do_set_active_conv([{ConvId, Pid}|Rest], ActiveId, UserId, SessId) ->
    Active = (ActiveId == ConvId),
    nkchat_conversation_obj:set_session_active(any, Pid, UserId, SessId, Active),
    do_set_active_conv(Rest, ActiveId, UserId, SessId).


%% @private
do_add_conv(ConvId, State) ->
    #?STATE{srv_id=SrvId, id=#obj_id_ext{obj_id=SessId}} = State,
    #?STATE{session=Session} = State,
    #session{user_id=UserId, conv_pids=Convs1} = Session,
    case nkchat_conversation_obj:add_session(SrvId, ConvId, UserId, SessId, #{}) of
        {ok, Pid} ->
            monitor(process, Pid),
            Convs2 = Convs1#{ConvId => Pid},
            Session2 = Session#session{conv_pids=Convs2},
            {ok, State#?STATE{session=Session2}};
        {error, Error} ->
            {error, Error}
    end.


%% @private
do_rm_conv(ConvId, State) ->
    case get_conv_pid(ConvId, State) of
        {ok, Pid} ->
            #?STATE{id=#obj_id_ext{obj_id=SessId}} = State,
            #?STATE{session=Session} = State,
            #session{user_id=UserId, conv_pids=ConvPids1, active_id=ActiveId} = Session,
            nkchat_conversation_obj:remove_session(any, Pid, UserId, SessId),
            ConvPids2 = maps:remove(Pid, ConvPids1),
            Session2 = Session#session{conv_pids=ConvPids2},
            Session3 = case ActiveId of
                ConvId ->
                    Session2#session{active_id=undefined};
                _ ->
                    Session2
            end,
            State2 = do_event({conversation_removed, ConvId}, State),
            {ok, State2#?STATE{session=Session3}};
        not_found ->
            {error, conversation_not_found}
    end.


%% @private
do_conversation_event({member_added, MemberId}, ConvId, State) ->
    {noreply, do_event({member_added, ConvId, MemberId}, State)};

do_conversation_event({member_removed, MemberId}, ConvId, #?STATE{session=Session}=State) ->
    State2 = case Session of
        #session{user_id=MemberId} ->
            {ok, S} = do_rm_conv(ConvId, State),
            S;
        _ ->
            State
    end,
    {noreply, do_event({member_removed, ConvId, MemberId}, State2)};

do_conversation_event({message_created, Msg}, ConvId, State) ->
    {noreply, do_event({message_created, ConvId, Msg}, State)};

do_conversation_event({message_updated, Msg}, ConvId, State) ->
    {noreply, do_event({message_updated, ConvId, Msg}, State)};

do_conversation_event({message_deleted, MsgId}, ConvId, State) ->
    {noreply, do_event({message_deleted, ConvId, MsgId}, State)};

do_conversation_event({counter_updated, Counter}, ConvId, State) ->
    {noreply, do_event({unread_counter_updated, ConvId, Counter}, State)};

do_conversation_event(_Event, _ConvId, State) ->
    % lager:error("SESS EV2"),
    {noreply, State}.


%% @private
get_conv_pid(ConvId, #?STATE{session=Session}) ->
    #session{conv_pids=Convs} = Session,
    case maps:find(ConvId, Convs) of
        {ok, Pid} ->
            {ok, Pid};
        error ->
            not_found
    end.

%% @private
do_event(Event, State) ->
    nkdomain_obj_util:event(Event, State).
