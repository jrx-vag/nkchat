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

-export([start/3, get_calls/1, launch_notifications/1]).
-export([invite/3, cancel_invite/1, accept_invite/3, reject_invite/1]).
-export([object_info/0, object_es_mapping/0, object_parse/2,
         object_api_syntax/2, object_api_cmd/2]).
-export([object_init/1, object_stop/2, object_send_event/2,
         object_sync_op/3, object_async_op/2]).
-export([object_admin_info/0, object_handle_info/2]).
-export([call_event/3, notify_fun/2]).

-export_type([event/0]).

-include("nkchat.hrl").
-include_lib("nkdomain/include/nkdomain.hrl").
-include_lib("nkdomain/include/nkdomain_debug.hrl").
-include_lib("nkevent/include/nkevent.hrl").


%% ===================================================================
%% Types
%% ===================================================================




-type event() ::
    {invite, InviteId::binary(), CallerId::binary(), nkchat_media_call_obj:invite_opts()} |
    {invite_removed, InviteId::binary(), Reason::nkservice:error()} |
    {invite_accepted, InviteId::binary(), CallId::binary(), nkchat_media_call_obj:accept_opts()} |
    {call_created, InviteId::binary(), CallId::binary(), #{}} |
    {call_hangup, CallId::binary(), Reason::nkservice:error()} |
    {new_candidate, CallId::binary(), nkchat_media_call_obj:candidate()} |
    {member_status, CallId::binary(), Member::binary(), nkchat_media_call_obj:status()}.


-type start_opts() :: #{
    session_link => {module(), pid()},
    session_events => [binary()]
}.


%% ===================================================================
%% Public
%% ===================================================================


%% @doc Creates a new session
-spec start(nkdomain:id(), nkdomain:id(), start_opts()) ->
    {ok, nkdomain:obj_id(), pid()} | {error, term()}.

start(DomainId, UserId, Opts) ->
    Obj = #{
        type => ?MEDIA_SESSION,
        domain_id => DomainId,
        parent_id => UserId,
        created_by => UserId,
        active => true,
        ?MEDIA_SESSION => #{}
    },
    Opts2 = maps:with([session_link, session_events], Opts),
    case nkdomain_obj_make:create(Obj, Opts2) of
        {ok, #obj_id_ext{obj_id=SessId, pid=Pid}, _} ->
            {ok, SessId, Pid};
        {error, Error} ->
            {error, Error}
    end.


%% @doc
-spec invite(nkdomain:id(), nkdomain:id(), nkchat_media_call_obj:invite_opts()|#{call_name=>Name}) ->
    {ok, InviteId::nkdomain:obj_id()} | {error, term()}.

invite(Id, Callee, InviteOpts) ->
    case nkdomain_lib:load(Callee) of
        #obj_id_ext{type = ?DOMAIN_USER, obj_id=CalleeId} ->
            nkdomain_obj:sync_op(Id, {?MODULE, invite, CalleeId, InviteOpts});
        {error, object_not_found} ->
            {error, user_not_found};
        {error, Error} ->
            {error, Error}
    end.


%% @doc
cancel_invite(InviteId) ->
    nkchat_media_call_obj:cancel_invite(InviteId).


%% @doc Accepts a invitation notification
-spec accept_invite(nkdomain:id(), nkdomain:id(),  nkchat_media_call_obj:accept_opts()) ->
    ok | {error, term()}.

accept_invite(SessId, InviteId, AcceptOpts) ->
    nkdomain_obj:sync_op(SessId, {?MODULE, accept_invite, InviteId, AcceptOpts}).


%% @doc Rejects a invitation notification
-spec reject_invite(nkdomain:id()) ->
    ok | {error, term()}.

reject_invite(InviteId) ->
    nkchat_media_call_obj:reject_invite(InviteId).


%% @doc
launch_notifications(Id) ->
    nkdomain_obj:async_op(Id, {?MODULE, launch_notifications}).


%% @doc
-spec get_calls(nkdomain:id()) ->
    {ok, [nkdomain:obj_id()]} | {error, term()}.

get_calls(Id) ->
    nkdomain_obj:sync_op(Id, {?MODULE, get_calls}).


%% @private To be called from nkmedia_call_obj
call_event(Pid, CallId, Event) ->
    nkdomain_obj:async_op(Pid, {?MODULE, call_event, CallId, Event}).


%% @private To be called from nkdomain_user_obj
-spec notify_fun(pid(), nkdomain_user_obj:notify_msg()) ->
    any.

notify_fun(Pid, Notify) ->
    nkdomain_obj:async_op(Pid, {?MODULE, notify_fun, Notify}).




%% ===================================================================
%% nkdomain_obj behaviour
%% ===================================================================

-record(invite, {
    id :: nkdomain:obj_id(),
    pid :: pid(),
    role :: caller | callee
}).


-record(call, {
    id :: nkdomain:obj_id(),
    role :: caller | callee,
    pid :: pid(),
    monitor :: reference()
}).

-record(session, {
    invites = [] :: [#invite{}],
    calls = [] :: [#call{}]
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
        type_view_mod => nkchat_media_session_obj_type_view
    }.


%% @private
object_es_mapping() ->
    not_indexed.


%% @private
object_parse(_Mode, _Obj) ->
    #{}.


%% @private
object_api_syntax(Cmd, Syntax) ->
    nkchat_media_session_obj_syntax:syntax(Cmd, Syntax).


%% @private
object_api_cmd(Cmd, Req) ->
    nkchat_media_session_obj_cmd:cmd(Cmd, Req).


%% @private
object_send_event(Event, State) ->
    nkchat_media_session_obj_events:event(Event, State).


%% @private When the object is loaded, we make our cache
object_init(#obj_state{id=Id, parent_id=UserId, domain_id=DomainId}=State) ->
    #obj_id_ext{obj_id=SessId} = Id,
    Session = #session{
        invites = [],
        calls = []
    },
    State2 = State#obj_state{session=Session},
    Opts = #{notify_fun => fun ?MODULE:notify_fun/2},
    ok = nkdomain_user_obj:register_session(UserId, DomainId, ?MEDIA_SESSION, SessId, Opts),
    State4 = nkdomain_obj_util:link_to_session_server(?MODULE, State2),
    {ok, State4}.


%% @private
object_stop(_Reason, State) ->
    {ok, nkdomain_obj_util:unlink_from_session_server(?MODULE, State)}.


%% @private
object_sync_op({?MODULE, get_calls}, _From, #obj_state{session=Session}=State) ->
    #session{calls=Calls} = Session,
    {reply, {ok, [CallId || #call{id=CallId} <- Calls]}, State};

object_sync_op({?MODULE, invite, CalleeId, InviteOpts}, _From, #obj_state{session=Session}=State) ->
    case Session of
        #session{invites=[], calls=[]} ->
            case do_invite(CalleeId, InviteOpts, State) of
                {ok, InviteId, State2} ->
                    {reply, {ok, InviteId}, State2};
                {error, Error} ->
                    {reply, {error, Error}, State}
            end;
        #session{calls=[#call{id=CallId}|_]} ->
            {reply, {error, {call_is_active, CallId}}, State}
    end;

object_sync_op({?MODULE, accept_invite, InviteId, AcceptOpts}, _From, #obj_state{session=Session}=State) ->
    #obj_state{id=#obj_id_ext{obj_id=SessId}, parent_id=UserId} = State,
    case Session of
        #session{calls=[]} ->
            case nkchat_media_call_obj:accept_invite(InviteId, UserId, SessId, [callee], AcceptOpts) of
                {ok, CallId, CallPid} ->
                    State2 = do_add_call(CallId, CallPid, callee, State),
                    State3 =
                    {reply, {ok, CallId}, State2};
                {error, Error} ->
                    {reply, {error, Error}, State}
            end;
        #session{calls=[#call{id=CallId}|_]} ->
            {reply, {error, {call_is_active, CallId}}, State}
    end;

object_sync_op(_Op, _From, _State) ->
    continue.


%% @private
object_async_op({?MODULE, call_event, CallId, Event}, State) ->
    State2 = do_call_event(Event, CallId, State),
    {noreply, State2};

object_async_op({?MODULE, launch_notifications}, State) ->
    #obj_state{id=#obj_id_ext{obj_id=SessId}, parent_id=UserId} = State,
    nkdomain_user_obj:launch_session_notifications(UserId, SessId),
    {noreply, State};

object_async_op({?MODULE, notify_fun, {token_created, InviteId, Msg}}, State) ->
    case nkchat_media_call_obj:get_invite_token(Msg) of
        {ok, #{caller_id:=CallerId, invite_opts:=InviteOpts}} ->
            case nkdomain_lib:find(InviteId) of
                #obj_id_ext{pid=Pid} ->
                    State2 = do_add_invite(InviteId, Pid, callee, State),
                    State3 = do_event({invite, InviteId, CallerId, InviteOpts}, State2),
                    {noreply, State3};
                _ ->
                    ?LLOG(warning, "token ~s has failed", [InviteId], State),
                    {noreply, State}
            end;
        {error, _Error} ->
            ?LLOG(warning, "invalid token ~s", [InviteId], State),
            {noreply, State}
    end;

object_async_op({?MODULE, notify_fun, {token_removed, InviteId, Reason}}, State) ->
    State2 = do_rm_invite(InviteId, State),
    State3 = do_event({invite_removed, InviteId, Reason}, State2),
    {noreply, State3};

object_async_op(_Op, _State) ->
    continue.


%% @private
object_handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    Calls = do_get_calls(State),
    case lists:keyfind(Pid, #call.pid, Calls) of
        #call{id=CallId} ->
            ?LLOG(notice, "call down ~s", [CallId], State),
            State2 = do_rm_call(CallId, process_down, State),
            {noreply, State2};
        false ->
            Invites = do_get_invites(State),
            case lists:keyfind(Pid, #invite.pid, Invites) of
                #invite{id=InviteId} ->
                    State2 = do_rm_invite(InviteId, State),
                    {noreply, State2};
                false ->
                    continue
            end
    end;

object_handle_info(_Msg, _State) ->
    continue.



%% ===================================================================
%% Internal
%% ===================================================================

%% @private
do_invite(CalleeId, InviteOpts, State) ->
    #obj_state{domain_id=DomainId, parent_id=UserId, id=#obj_id_ext{obj_id=SessId}} = State,
    CallOpts1 = #{created_by => UserId},
    CallOpts2 = case InviteOpts of
        #{call_name:=Name} ->
            CallOpts1#{name=>Name};
        _ ->
            CallOpts1
    end,
    case nkchat_message_obj:create(DomainId, CallOpts2) of
        {ok, CallId, CallPid} ->
            case nkchat_media_call_obj:add_member(CallId, UserId, SessId, [caller]) of
                ok ->
                    case nkchat_media_call_obj:invite(CallId, UserId, CalleeId, InviteOpts) of
                        {ok, InviteId, InvitePid} ->
                            State2 = do_add_invite(InviteId, InvitePid, caller, State),
                            {ok, InviteId, State2};
                        {error, Error} ->
                            ?LLOG(warning, "error sending invite: ~p", [Error], State),
                            nkchat_media_call_obj:hangup_async(CallPid, session_error),
                            {error, Error}
                    end;
                {error, Error} ->
                    ?LLOG(warning, "error adding member: ~p", [Error], State),
                    nkchat_media_call_obj:hangup_async(CallPid, session_error),
                    {error, Error}
            end;
        {error, Error} ->
            ?LLOG(warning, "error creating call: ~p", [Error], State),
            {error, Error}
    end.


%% @private
update_status(#obj_state{id=#obj_id_ext{obj_id=SessId}, parent_id=UserId, session=Session}=State) ->
    Status = case Session of
        #session{invites=[], calls=[]} ->
            <<"normal">>;
        #session{calls=[]} ->
            <<"ringing">>;
        #session{} ->
            <<"in_call">>
    end,
    nkdomain_user_obj:update_presence(UserId, SessId, Status),
    State.



%% @private
do_call_event({call_hangup, Reason}, CallId, State) ->
    do_rm_call(CallId, Reason, State);

do_call_event({member_added, MemberId, Roles, _SessId, _Pid}, CallId, State) ->
    do_event({member_added, CallId, MemberId, Roles}, State);

do_call_event({member_removed, MemberId, Roles}, CallId, State) ->
    do_event({member_removed, CallId, MemberId, Roles}, State);

do_call_event({member_down, MemberId, Roles}, CallId, State) ->
    do_event({member_down, CallId, MemberId, Roles}, State);

do_call_event({new_candidate, Candidate}, CallId, State) ->
    do_event({new_candidate, CallId, Candidate}, State);

do_call_event({member_status, MemberId, Status}, CallId, State) ->
    do_event({member_status, CallId, MemberId, Status}, State);

do_call_event(_Event, _CallId, State) ->
    lager:error("SESS EV2: ~p", [_Event]),
    State.


%% @private
do_event(Event, State) ->
    nkdomain_obj_util:event(Event, State).


%% @private
do_get_invites(#obj_state{session=Session}) ->
    #session{invites=Calls} = Session,
    Calls.


%% @private
do_add_invite(InviteId, Pid, Role, #obj_state{session=Session} = State) ->
    Invites = do_get_invites(State),
    _Mon = monitor(process, Pid),
    Invite = #invite{id=InviteId, role=Role, pid=Pid},
    Invites2 = lists:keystore(InviteId, #invite.id, Invites, Invite),
    Session2 = Session#session{invites=Invites2},
    State2 = State#obj_state{session=Session2},
    update_status(State2).


%% @private
do_rm_invite(InviteId, #obj_state{session=Session} = State) ->
    Invites = do_get_invites(State),
    case lists:keytake(InviteId, #invite.id, Invites) of
        {value, #invite{}, Invites2} ->
            Session2 = Session#session{invites=Invites2},
            State2 = State#obj_state{session=Session2},
            State3 = do_event({invite_hangup, InviteId}, State2),
            update_status(State3);
        false ->
            State
    end.


%% @private
do_get_calls(#obj_state{session=Session}) ->
    #session{calls=Calls} = Session,
    Calls.


%% @private
do_add_call(CallId, Pid, Role, #obj_state{session=Session} = State) ->
    Calls = do_get_calls(State),
    Mon = monitor(process, Pid),
    Call = #call{id=CallId, role=Role, pid=Pid, monitor=Mon},
    Calls2 = lists:keystore(CallId, #call.id, Calls, Call),
    Session2 = Session#session{calls=Calls2},
    State2 = State#obj_state{session=Session2},
    update_status(State2).


%% @private
do_rm_call(CallId, Reason, #obj_state{session=Session} = State) ->
    Calls = do_get_calls(State),
    case lists:keytake(CallId, #call.id, Calls) of
        {value, #call{monitor=Mon}, Calls2} ->
            nklib_util:demonitor(Mon),
            Session2 = Session#session{calls=Calls2},
            State2 = State#obj_state{session=Session2},
            State3 = do_event({call_hangup, CallId, Reason}, State2),
            update_status(State3);
        false ->
            State
    end.
