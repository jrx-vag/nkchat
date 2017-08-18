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
-module(nkchat_media_session_obj).
-behavior(nkdomain_obj).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([start/4, get_calls/2, get_call_info/3]).
-export([invite/4, cancel_invite/3, accept_invite/4, reject_invite/3]).
-export([object_info/0, object_es_mapping/0, object_parse/3,
         object_api_syntax/2, object_api_cmd/2]).
-export([object_init/1, object_stop/2, object_send_event/2,
         object_sync_op/3, object_async_op/2]).
-export([object_admin_info/0, object_link_down/2]).
-export([call_event/3, notify_fun/5]).

-export_type([event/0]).

-include("nkchat.hrl").
-include_lib("nkdomain/include/nkdomain.hrl").
-include_lib("nkdomain/include/nkdomain_debug.hrl").
-include_lib("nkevent/include/nkevent.hrl").

-define(DEFAULT_INVITE_TTL, 3*60).     % Secs


%% ===================================================================
%% Types
%% ===================================================================


%%-type meta() ::
%%    #{
%%        user_id => nkdomain:obj_id()
%%    }.

-type call_opts() :: nkchat_media_call_obj:call_opts().


-type event() ::
    {invite, InviteId::binary(), CallerId::binary(), call_opts()} |
    {invite_removed, InviteId::binary(), #{reason=>nkservice:error()}} |
    {invite_accepted, InviteId::binary(), CallId::binary(), call_opts()}.


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
        type => ?MEDIA_SESSION,
        domain_id => DomainId,
        parent_id => UserId,
        created_by => UserId,
        active => true,
        ?MEDIA_SESSION => #{}
    },
    Opts2 = maps:with([session_link, session_events], Opts),
    case nkdomain_obj_make:create(SrvId, Obj, Opts2) of
        {ok, #obj_id_ext{obj_id=SessId, pid=Pid}, _} ->
            {ok, SessId, Pid};
        {error, Error} ->
            {error, Error}
    end.


%% @doc
-spec invite(nkservice:id(), nkdomain:id(), nkdomain:id(), call_opts()) ->
    {ok, CallId::nkdomain:obj_id(), InviteId::nkdomain:obj_id()} | {error, term()}.

invite(SrvId, Id, Callee, CallOpts) ->
    case nkdomain_lib:load(SrvId, Callee) of
        #obj_id_ext{type = ?DOMAIN_USER, obj_id=CalleeId} ->
            nkdomain_obj:sync_op(SrvId, Id, {?MODULE, invite, CalleeId, CallOpts});
        {error, object_not_found} ->
            {error, user_not_found};
        {error, Error} ->
            {error, Error}
    end.


%% @doc
cancel_invite(SrvId, Id, InviteId) ->
    case nkdomain_token_obj:consume_token(SrvId, InviteId, cancelled) of
        {ok, #{data:=Data}} ->
            nkdomain_obj:sync_op(SrvId, Id, {?MODULE, cancel_invite, InviteId, Data});
        {error, Error} ->
            {error, Error}
    end.


%% @doc
-spec get_calls(nkservice:id(), nkdomain:id()) ->
    {ok, [nkdomain:obj_id()]} | {error, term()}.

get_calls(SrvId, Id) ->
    nkdomain_obj:sync_op(SrvId, Id, {?MODULE, get_calls}).


%% @doc
-spec get_call_info(nkservice:id(), nkdomain:id(), nkdomain:id()) ->
    {ok, map()} | {error, term()}.

get_call_info(SrvId, Id, Call) ->
    case nkdomain_lib:find(SrvId, Call) of
        {ok, _Type, CallId, _Pid} ->
            nkdomain_obj:sync_op(SrvId, Id, {?MODULE, get_call_info, CallId});
        {error, Error} ->
            {error, Error}
    end.


%% @doc Accepts a invitation notification
-spec accept_invite(nkservice:id(), nkdomain:id(), nkdomain:id(), call_opts()) ->
    ok | {error, term()}.

accept_invite(SrvId, SessId, InvId, CallOpts) ->
    case nkdomain_token_obj:consume_token(SrvId, InvId, accepted) of
        {ok, #{data:=Data}} ->
            nkdomain_obj:sync_op(SrvId, SessId, {?MODULE, accept_invite, InvId, Data, CallOpts});
        {error, Error} ->
            {error, Error}
    end.


%% @doc Rejects a invitation notification
-spec reject_invite(nkservice:id(), nkdomain:id(), nkdomain:id()) ->
    ok | {error, term()}.

reject_invite(SrvId, SessId, InvId) ->
    case nkdomain_token_obj:consume_token(SrvId, InvId, accepted) of
        {ok, #{data:=Data}} ->
            nkdomain_obj:sync_op(SrvId, SessId, {?MODULE, reject_invite, InvId, Data});
        {error, Error} ->
            {error, Error}
    end.



%% @private To be called from nkdomain_user_obj
call_event(Pid, CallId, Event) ->
    lager:notice("NKLOG media sess call event ~p", [Event]),
    nkdomain_obj:async_op(any, Pid, {?MODULE, call_event, CallId, Event}).


%% @private To be called from nkdomain_user_obj
notify_fun(_SessId, Pid, TokenId, Msg, Op) ->
    % lager:error("NKLOG SESS FUN ~p ~p", [Op, Msg]),
    nkdomain_obj:async_op(any, Pid, {?MODULE, notify, TokenId, Msg, Op}).



%% ===================================================================
%% nkdomain_obj behaviour
%% ===================================================================

-record(invite, {
    id :: nkdomain:obj_id(),
    token_pid :: pid(),
    call_opts :: call_opts()
}).

-record(call, {
    id :: nkdomain:obj_id(),
    pid :: pid()
}).

-record(session, {
    user_id :: nkdomain:obj_id(),
    invites2 = [] :: [#invite{}],
    calls2 = [] :: [#call{}]
}).



%% @private
object_info() ->
    #{
        type => ?MEDIA_SESSION,
        stop_after_disabled => true,
        remove_after_stop => true
    }.


%% @doc
object_admin_info() ->
    #{
        class => session,
        weight => 2000,
        tree_id => <<"domain_tree_sessions_media.sessions">>
    }.


%% @private
object_es_mapping() ->
    not_indexed.


%% @private
object_parse(_SrvId, _Mode, _Obj) ->
    #{}.


%% @private
object_api_syntax(Cmd, Syntax) ->
    nkchat_media_session_obj_syntax:api(Cmd, Syntax).


%% @private
object_api_cmd(Cmd, Req) ->
    nkchat_media_session_obj_cmd:cmd(Cmd, Req).


%% @private
object_send_event(Event, State) ->
    nkchat_media_session_obj_events:event(Event, State).


%% @private When the object is loaded, we make our cache
object_init(#?STATE{id=Id, obj=Obj, domain_id=DomainId}=State) ->
    #obj_id_ext{srv_id=SrvId, obj_id=SessId} = Id,
    #{parent_id := UserId} = Obj,
    Session = #session{
        user_id = UserId
    },
    State2 = State#?STATE{session=Session},
%%    {ok, Calls1} = nkchat_conversation_obj:find_member_conversations(SrvId, DomainId, UserId),
%%    State3 = lists:foldl(
%%        fun({CallId, _Type}, Acc) ->
%%            case do_add_conv(CallId, Acc) of
%%                {ok, Acc2} ->
%%                    Acc2;
%%                {error, Error} ->
%%                    ?LLOG(warning, "could not load conversation ~s: ~p", [CallId, Error], State),
%%                    Acc
%%            end
%%        end,
%%        State2,
%%        Calls1),
    Opts = #{notify_fun => fun ?MODULE:notify_fun/5},
    ok = nkdomain_user_obj:register_session(SrvId, UserId, DomainId, ?MEDIA_SESSION, SessId, Opts),
    State4 = nkdomain_obj_util:link_to_session_server(?MODULE, State2),
    {ok, State4}.


%% @private
object_stop(_Reason, State) ->
    {ok, nkdomain_obj_util:unlink_from_session_server(?MODULE, State)}.


%% @private
object_sync_op({?MODULE, get_calls}, _From, #?STATE{session=Session}=State) ->
    #session{calls2=Calls} = Session,
    {reply, {ok, [CallId || #call{id=CallId} <- Calls]}, State};

object_sync_op({?MODULE, invite, CalleeId, CallOpts}, _From, State) ->
    case do_invite(CalleeId, CallOpts, State) of
        {ok, TokenId, State2} ->
            {reply, {ok, TokenId}, State2};
        {error, Error} ->
            {reply, {error, Error}, State}
    end;

object_sync_op({?MODULE, cancel_invite, InviteId, _Data}, _From, State) ->
    Invites = get_invites(State),
    case lists:keymember(InviteId, #invite.id, Invites) of
        true ->
            State2 = rm_invite(InviteId, State),
            State3 = do_event({invite_removed, InviteId, #{reason=>caller_cancelled}}, State2),
            {reply, ok, State3};
        false ->
            {reply, {error, invite_not_found}, State}
    end;

object_sync_op({?MODULE, accept_invite, InviteId, Data, CallOpts}, _From, State) ->
    case Data of
        #{
            ?MEDIA_SESSION := #{
                <<"invite_op">> := #{
                    <<"caller_session_id">> := SessId
                }
            }
        } ->
            #?STATE{srv_id=SrvId, parent_id=UserId} = State,
            % Avoid user detecting the going down of token
            nkdomain_user_obj:remove_notification(SrvId, UserId, InviteId, call_accepted),
            Reply = nkdomain_obj:sync_op(SrvId, SessId, {?MODULE, remote_accept_invite, InviteId, CallOpts}),
            {reply, Reply, State};
        _ ->
            {reply, {error, operation_token_invalid}, State}
    end;

object_sync_op({?MODULE, reject_invite, InviteId, Data}, _From, State) ->
    case Data of
        #{
            ?MEDIA_SESSION := #{
                <<"invite_op">> := #{
                    <<"caller_session_id">> := SessId
                }
            }
        } ->
            #?STATE{srv_id=SrvId, parent_id=UserId} = State,
            % Avoid user detecting the going down of token
            nkdomain_user_obj:remove_notification(SrvId, UserId, InviteId, call_rejected),
            Reply = nkdomain_obj:sync_op(SrvId, SessId, {?MODULE, remote_reject_invite, InviteId}),
            {reply, Reply, State};
        _ ->
            {reply, {error, operation_token_invalid}, State}
    end;

object_sync_op({?MODULE, remote_accept_invite, InviteId, CallOpts}, _From, State) ->
    Invites = get_invites(State),
    case lists:keymember(InviteId, #invite.id, Invites) of
        true ->
            case do_accept_invite(InviteId, CallOpts, State) of
                {ok, CallId, State2} ->
                    {reply, {ok, CallId}, State2};
                {error, Error, State2} ->
                    {reply, {error, Error}, State2}
            end;
        false ->
            {reply, {error, invite_not_found}, State}
    end;

object_sync_op({?MODULE, remote_reject_invite, InviteId}, _From, State) ->
    Invites = get_invites(State),
    case lists:keymember(InviteId, #invite.id, Invites) of
        true ->
            State2 = rm_invite(InviteId, State),
            State3 = do_event({invite_removed, InviteId, #{reason=>callee_rejected}}, State2),
            {reply, ok, State3};
        false ->
            {error, invite_not_found}
    end;

object_sync_op(_Op, _From, _State) ->
    continue.


%% @private
object_async_op({?MODULE, call_event, CallId, Event}, State) ->
    State2 = do_call_event(Event, CallId, State),
    {noreply, State2};

object_async_op({?MODULE, notify, InviteId, Msg, Op}, State) ->
    case Msg of
        #{
            ?MEDIA_SESSION := #{
                <<"invite_op">> := #{
                    <<"caller_id">> := CallerId,
                    <<"caller_session_id">> := _SessId,
                    <<"callee_id">> := _CalleeId,
                    <<"call_opts">> := CallOpts
                }
            }
        } ->
            State2 = case Op of
                created ->
                    do_event({invite, InviteId, CallerId, CallOpts}, State);
                {removed, Reason} ->
                    do_event({invite_removed, InviteId, #{reason=>Reason}}, State)
            end,
            {noreply, State2};
        _ ->
            ?LLOG(warning, "unexpected notify: ~p", [Msg], State),
            {noreply, State}
    end;

object_async_op(_Op, _State) ->
    continue.


%% @private
object_link_down({usage, {?MODULE, invite, InviteId, _Pid}}, State) ->
    ?LLOG(notice, "invite token down ~s", [InviteId], State),
    State2 = rm_invite(InviteId, State),
    State3 = do_event({invite_removed, InviteId, #{reason=>timeout}}, State2),
    {ok, State3};

object_link_down(_Link, State) ->
    {ok, State}.


%% ===================================================================
%% Internal
%% ===================================================================


do_invite(CalleeId, CallOpts, State) ->
    #?STATE{srv_id=SrvId, domain_id=DomainId, parent_id=CallerId, id=#obj_id_ext{obj_id=SessId}} = State,
    Op1= #{
        ?MEDIA_SESSION => #{
            <<"invite_op">> => #{
                <<"caller_id">> => CallerId,
                <<"caller_session_id">> => SessId,
                <<"callee_id">> => CalleeId,
                <<"call_opts">> => CallOpts
            }
        }
    },
    case nkdomain_user_obj:add_notification_op(SrvId, CalleeId, ?MEDIA_SESSION, #{}, Op1) of
        {ok, _MemberId, Op2} ->
            TTL = case CallOpts of
                #{ttl:=TTL0} when is_integer(TTL0), TTL0 > 0 ->
                    TTL0;
                _ ->
                    ?DEFAULT_INVITE_TTL
            end,
            Opts = #{ttl => TTL},
            case
                nkdomain_token_obj:create(SrvId, DomainId, CalleeId, CalleeId, <<"media.call">>, Opts, Op2)
            of
                {ok, InviteId, Pid, _Secs, _Unknown} ->
                    State2 = add_invite(InviteId, Pid, CallOpts, State),
                    {ok, InviteId, State2};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.


do_accept_invite(InviteId, CallOpts, State) ->
    #?STATE{srv_id=SrvId, domain_id=DomainId, parent_id=CallerId} = State,
    State2 = rm_invite(InviteId, State),
    case nkchat_media_call_obj:create(SrvId, DomainId, <<>>, CallerId, one2one) of
        {ok, CallId, CallPid} ->
            State3 = add_call(CallId, CallPid, State2),
            State4 = do_event({invite_accepted, InviteId, CallId, CallOpts}, State3),
            {ok, CallId, CallPid, State4};
        {error, Error} ->
            ?LLOG(warning, "could not created call: ~p", [Error], State),
            State3 = do_event({invite_removed, InviteId, internal_error}, State2),
            {error, internal_error, State3}
    end.



%% @private

%%
%%do_call_event({counter_updated, Counter}, CallId, State) ->
%%    do_event({unread_counter_updated, CallId, Counter}, State);

do_call_event(_Event, _CallId, State) ->
    lager:error("SESS EV2"),
    State.


%% @private
do_event(Event, State) ->
    nkdomain_obj_util:event(Event, State).


%% @private
get_invites(#?STATE{session=Session}) ->
    #session{invites2=Invites} = Session,
    Invites.


%% @private
add_invite(InviteId, Pid, CallOpts, #?STATE{session=Session}=State) ->
    Invites = get_invites(State),
    Invite = #invite{id=InviteId, token_pid=Pid, call_opts=CallOpts},
    Invites2 = lists:keystore(InviteId, #invite.id, Invites, Invite),
    Session2 = Session#session{invites2=Invites2},
    State2 = State#?STATE{session=Session2},
    nkdomain_obj:links_add(usage, {?MODULE, invite, InviteId, Pid}, State2).


%% @private
rm_invite(InviteId, #?STATE{session=Session}=State) ->
    Invites = get_invites(State),
    case lists:keytake(InviteId, #invite.id, Invites) of
        {value, #invite{token_pid=TokenPid}, Invites2} ->
            Session2 = Session#session{invites2=Invites2},
            State2 = State#?STATE{session=Session2},
            nkdomain_obj:links_remove(usage, {?MODULE, invite, InviteId, TokenPid}, State2);
        error ->
            State
    end.


%% @private
get_calls(#?STATE{session=Session}) ->
    #session{calls2=Calls} = Session,
    Calls.


%% @private
add_call(CallId, Pid, #?STATE{session=Session}=State) ->
    Calls = get_calls(State),
    Call = #call{id=CallId, pid=Pid},
    Calls2 = lists:keystore(CallId, #call.id, Calls, Call),
    Session2 = Session#session{calls2=Calls2},
    State2 = State#?STATE{session=Session2},
    nkdomain_obj:links_add(usage, {?MODULE, call, CallId, Pid}, State2).
