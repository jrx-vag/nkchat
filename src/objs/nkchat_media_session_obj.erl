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

%% Invite - Caller side
%% --------------------
%%
%% - API calls invite/3 to start a new 'direct' call. It should be in no active call.
%% - It creates a call with type 'direct' and call nkchat_media_call_obj:invite/5 that
%%     - adds a member to the call
%%     - creates and launches "invite token" to the callee
%%     - starts a new media session
%% - We get the media session id and store the call info at the session
%% - The call sends the media_ringing_out event and we wait for the answer
%%     - if the call stops, it will send media_stopped event and call_hangup
%%     - if the call fails, it is detected and the same
%%     - if we call cancel_invite, we call stop_media and media_stopped will be received
%%     - if the calling timeout fires, the same
%%     - if the call is answered, we will receive media_answered event
%%
%% Callee side
%% -----------
%%
%% - Callee receives the invite token through the notify_fun and sends invite event
%% - If reject_invite is called, and the token is still valid, the media session is stopped
%%     - caller received media_stopped
%%     - no event is received
%% - If accept_invite is called, and the token is valid


-module(nkchat_media_session_obj).
-behavior(nkdomain_obj).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([start/3, get_call_info/1, launch_notifications/1]).
-export([invite/3, cancel_invite/2, accept_invite/3, reject_invite/2]).
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
    % Own events
    {call_created, CallId::binary()} |
    {media_invite, MediaId::nkdomain:obj_id(), CallId::nkdomain:obj_id(), CallerId::nkdomain:obj_id(),
             InviteOpts::nkchat_media_call_obj:invite_opts()} |

    % Events from call
    {media_ringing_out, MediaId::nkdomain:obj_id(), CallId::nkdomain:obj_id()} |
    {media_answered,  MediaId::nkdomain:obj_id(), CallId::nkdomain:obj_id(), nkchat_media_call_obj:accept_opts()} |
    {media_started, MediaId::nkdomain:obj_id(), CallId::nkdomain:obj_id()} |
    {media_stopped, MediaId::nkdomain:obj_id(), CallId::nkdomain:obj_id(), Reason::term()} |
    {new_candidate, CallId::nkdomain:obj_id(), nkchat_media_call_obj:candidate()} |
    {session_status, SessId::nkdomain:obj_id(), CallId::nkdomain:obj_id(), nkchat_media_call_obj:session_status()} |
    {call_status, CallId::nkdomain:obj_id(), nkchat_media_call_obj:call_status()} |
    {session_added, SessId::nkdomain:obj_id(), User::nkdomain:obj_id(), CallId::nkdomain:obj_id(), #{}} |
    {session_removed, SessId::nkdomain:obj_id(), User::nkdomain:obj_id(), CallId::nkdomain:obj_id(), #{}} |
    {call_hangup, Reason::nkservice:error(), CallId::nkdomain:obj_id()}.



-type start_opts() :: #{
    session_link => {module(), pid()},
    session_events => [binary()]
}.


-type invite_opts() ::
    nkchat_media_call_obj:invite_opts() |
    #{call_name=>binary()}.


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
-spec invite(nkdomain:id(), nkdomain:id(), invite_opts()) ->
    {ok, MediaId::nkdomain:obj_id()} | {error, term()}.

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
cancel_invite(Id, MediaId) ->
    nkdomain_obj:sync_op(Id, {?MODULE, cancel_invite, MediaId}).


%% @doc Accepts a invitation notification
-spec accept_invite(nkdomain:id(), nkdomain:id(),  nkchat_media_call_obj:accept_opts()) ->
    ok | {error, term()}.

accept_invite(SessId, MediaId, AcceptOpts) ->
    nkdomain_obj:sync_op(SessId, {?MODULE, accept_invite, MediaId, AcceptOpts}).


%% @doc Rejects a invitation notification
-spec reject_invite(nkdomain:id(), nkdomain:obj_id()) ->
    ok | {error, term()}.

reject_invite(_SessId, MediaId) ->
    case consume_token(MediaId, reject_invite) of
        {ok, CallId} ->
            nkchat_media_call_obj:stop_media(CallId, MediaId, callee_rejected);
        {error, _} ->
            ok
    end.


%% @doc
launch_notifications(Id) ->
    nkdomain_obj:async_op(Id, {?MODULE, launch_notifications}).


%% @doc
-spec get_call_info(nkdomain:id()) ->
    {ok, [nkdomain:obj_id()]} | {error, term()}.

get_call_info(Id) ->
    nkdomain_obj:sync_op(Id, {?MODULE, get_call_info}).


%% @private To be called from nkmedia_call_obj
-spec call_event(pid(), nkdomain:obj_id(),
                 nkchat_media_call_obj:session_event() | nkchat_media_call_obj:member_session_event()) ->
    ok.

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

-record(session, {
    media_id = <<>> :: nkdomain:obj_id(),
    media_status = no_media :: no_media | ringing | answered,
    call_id = <<>> :: nkdomain:obj_id(),
    call_monitor :: reference()
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
    Session = #session{},
    State2 = State#obj_state{session=Session},
    Opts = #{notify_fun => fun ?MODULE:notify_fun/2},
    ok = nkdomain_user_obj:register_session(UserId, DomainId, ?MEDIA_SESSION, SessId, Opts),
    State4 = nkdomain_obj_util:link_to_session_server(?MODULE, State2),
    {ok, State4}.


%% @private
object_stop(_Reason, State) ->
    {ok, nkdomain_obj_util:unlink_from_session_server(?MODULE, State)}.


%% @private
object_sync_op({?MODULE, get_call_info}, _From, #obj_state{session=Session}=State) ->
    case Session of
        #session{media_status=no_media} ->
            {reply, {ok, #{media_status=>no_media}}, State};
        #session{media_status=Status, call_id=CallId, media_id=MediaId} ->
            {reply, {ok, #{media_status=>Status, media_id=>MediaId, call_id=>CallId}}, State}
    end;

object_sync_op({?MODULE, invite, CalleeId, InviteOpts}, _From, #obj_state{session=Session}=State) ->
    case Session of
        #session{media_status=no_media} ->
            case do_invite(CalleeId, InviteOpts, State) of
                {ok, MediaId, _CallId, State2} ->
                    {reply, {ok, MediaId}, State2};
                {error, Error} ->
                    {reply, {error, Error}, State}
            end;
        #session{media_id=MediaId} ->
            {reply, {error, {media_is_active, MediaId}}, State}
    end;

object_sync_op({?MODULE, accept_invite, MediaId, CallId, AcceptOpts}, _From, #obj_state{session=Session}=State) ->
    case Session of
        #session{media_status=no_media} ->
            case consume_token(MediaId, call_accepted) of
                {ok, CallId} ->
                    case do_accept(MediaId, CallId, AcceptOpts, State) of
                        {ok, State2} ->
                            {reply, ok, State2};
                        {error, Error} ->
                            {reply, {error, Error}, State}
                    end;
                {error, Error} ->
                    {reply, {error, Error}, State}
            end;
        #session{media_id=MediaId} ->
            {reply, {error, {media_is_active, MediaId}}, State}
    end;

object_sync_op({?MODULE, cancel_invite, MediaId}, _From, #obj_state{session=Session}=State) ->
    Reply = case Session of
        #session{call_id=CallId, media_id=MediaId} ->
            nkchat_media_call_obj:stop_media(CallId, MediaId, caller_cancelled);
        _ ->
            {error, media_unknown, State}
    end,
    {reply, Reply, State};

object_sync_op({?MODULE, reject_invite, MediaId}, _From, #obj_state{session=Session}=State) ->
    Reply = case Session of
        #session{call_id=CallId, media_id=MediaId} ->
            nkchat_media_call_obj:stop_media(CallId, MediaId, callee_rejected);
        _ ->
            {error, media_unknown, State}
    end,
    {reply, Reply, State};



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

object_async_op({?MODULE, notify_fun, {token_created, MediaId, Msg}}, State) ->
    case nkchat_media_call_obj:get_invite_token(Msg) of
        {ok, #{call_id:=CallId, caller_id:=CallerId, invite_opts:=InviteOpts}} ->
            State2 = do_event({media_invite, MediaId, CallId, CallerId, InviteOpts}, State),
            {noreply, State2};
        {error, Error} ->
            ?LLOG(warning, "invalid token ~s: ~p", [MediaId, Error], State),
            {noreply, State}
    end;

object_async_op({?MODULE, notify_fun, {token_removed, MediaId, Msg, Reason}}, State) ->
    case nkchat_media_call_obj:get_invite_token(Msg) of
        {ok, #{call_id:=CallId}} ->
            State2 = do_event({media_stopped, MediaId, CallId, Reason}, State),
            {noreply, State2};
        {error, Error} ->
            ?LLOG(warning, "invalid token ~s: ~p", [MediaId, Error], State),
            {noreply, State}
    end;

object_async_op(_Op, _State) ->
    continue.


%% @private
object_handle_info({'DOWN', Ref, process, _Pid, _Reason}, State) ->
    case State of
        #obj_state{session=#session{call_id=CallId, call_monitor=Ref}} ->
            ?LLOG(notice, "call down ~s", [CallId], State),
            {noreply, do_call_down(CallId, process_down, State)};
        _ ->
            continue
    end;

object_handle_info(_Msg, _State) ->
    continue.



%% ===================================================================
%% Internal
%% ===================================================================

%% @private
consume_token(MediaId, Reason) ->
    case nkdomain_token_obj:consume_token(MediaId, Reason) of
        {ok, Msg} ->
            case nkchat_media_call_obj:get_invite_token(Msg) of
                {ok, #{call_id:=CallId}} ->
                    {ok, CallId};
                {error, _Error} ->
                    {error, token_invalid}
            end;
        {error, _} ->
            {error, token_invalid}
    end.


%% @private
do_invite(CalleeId, InviteOpts, State) ->
    #obj_state{
        domain_id = DomainId,
        parent_id = UserId,
        id = #obj_id_ext{obj_id=SessId}
    } = State,
    CallOpts1 = #{
        type => direct,
        parent_id => UserId,
        created_by => UserId
    },
    CallOpts2 = case InviteOpts of
        #{call_name:=Name} ->
            CallOpts1#{name=>Name};
        _ ->
            CallOpts1
    end,
    case nkchat_media_call_obj:create(DomainId, CallOpts2) of
        {ok, CallId, CallPid} ->
            State2 = do_event({call_created, CallId}, State),
            case nkchat_media_call_obj:invite(CallId, SessId, UserId, CalleeId, InviteOpts) of
                {ok, MediaId} ->
                    % Event media_ringing_out will come immediately from the call
                    % We however store it already to avoid a second quick call from the client
                    State3 = do_add_media(MediaId, ringing, CallId, CallPid, State2),
                    {ok, MediaId, CallId, State3};
                {error, Error} ->
                    ?LLOG(warning, "error sending invite: ~p", [Error], State),
                    nkchat_media_call_obj:hangup_async(CallPid, invite_error),
                    {error, Error}
            end;
        {error, Error} ->
            ?LLOG(warning, "error creating call: ~p", [Error], State),
            {error, Error}
    end.


%% @private
do_accept(MediaId, CallId, AcceptOpts, #obj_state{id=#obj_id_ext{obj_id=SessId}, parent_id=UserId} = State) ->
    case nkchat_media_call_obj:answer_media(CallId, MediaId, SessId, UserId, AcceptOpts) of
        {ok, CallId, CallPid} ->
            State2 = do_add_media(MediaId, answered, CallId, CallPid, State),
            {ok, State2};
        {error, Error} ->
            {error, Error}
    end.


%% @private
update_presence(State) ->
    #obj_state{
        id = #obj_id_ext{obj_id=SessId},
        parent_id = UserId,
        session = #session{media_status=Status}
    }=State,
    nkdomain_user_obj:update_presence(UserId, SessId, nklib_util:to_binary(Status)),
    State.


%% @private
do_add_media(MediaId, Status, CallId, CallPid, #obj_state{session=Session} = State) ->
    Session2 = Session#session{
        media_status = Status,
        call_id = CallId,
        call_monitor = monitor(process, CallPid),
        media_id = MediaId
    },
    State2 = State#obj_state{session=Session2},
    update_presence(State2).


%% @private
do_rm_media(#obj_state{session=Session}=State) ->
    #session{call_monitor=Mon} = Session,
    demonitor(Mon),
    Session2 = Session#session{
        media_status = no_media,
        media_id = <<>>,
        call_id = <<>>,
        call_monitor = undefined
    },
    State2 = State#obj_state{session=Session2},
    update_presence(State2).


%% @private
do_update_call(Status, #obj_state{session=Session}=State) ->
    Session2 = Session#session{media_status=Status},
    State2 = State#obj_state{session=Session2},
    update_presence(State2).


%% @private
do_call_down(Reason, CallId, #obj_state{session=Session}=State) ->
    case Session of
        #session{call_id=CallId, media_id=MediaId} ->
            State2 = do_rm_media(State),
            State3 = do_event({media_stopped, MediaId, CallId, Reason}, State2),
            do_event({call_hangup, CallId, Reason}, State3);
        _ ->
            % Media stopped should have been sent already
            do_event({call_hangup, CallId, Reason}, State)
    end.


%% @private
do_call_event({media_ringing_out, MediaId}, CallId, #obj_state{session=Session}=State) ->
    #session{call_id=CallId, media_status=ringing, media_id=MediaId} = Session,
    do_event({media_ringing_out, MediaId, CallId}, State);

do_call_event({media_answered, MediaId, AcceptOpts}, CallId, #obj_state{session=Session}=State) ->
    #session{call_id=CallId, media_id=MediaId, media_status=ringing} = Session,
    State2 = do_update_call(answered, State),
    do_event({media_answered, MediaId, CallId, AcceptOpts}, State2);

do_call_event({media_started, MediaId}, CallId, #obj_state{session=Session}=State) ->
    #session{call_id=CallId, media_id=MediaId, media_status=answered} = Session,
    do_event({media_started, MediaId, CallId}, State);

do_call_event({media_stopped, MediaId, Reason}, CallId, #obj_state{session=Session}=State) ->
    case Session of
        #session{call_id=CallId, media_id=MediaId} ->
            State2 = do_rm_media(State),
            do_event({media_stopped, MediaId, CallId, Reason}, State2);
        _ ->
            ?LLOG(warning, "received media_stopped for unknown_call ~s", [CallId], State),
            State
    end;

do_call_event({new_candidate, Candidate}, CallId, State) ->
    do_event({new_candidate, CallId, Candidate}, State);

do_call_event({call_status, Status}, CallId, State) ->
    do_event({call_status, CallId, Status}, State);

do_call_event({session_added, SessId, MemberId, Data}, CallId, State) ->
    do_event({session_added, SessId, MemberId, CallId, Data}, State);

do_call_event({session_removed, SessId, MemberId, Data}, CallId, State) ->
    do_event({session_removed, SessId, MemberId, CallId, Data}, State);

do_call_event({session_status, SessId, Status}, CallId, State) ->
    do_event({session_status, SessId, CallId, Status}, State);

do_call_event({call_hangup, Reason}, CallId, State) ->
    do_call_down(CallId, Reason, State);

do_call_event(_Event, _CallId, State) ->
    lager:error("SESS EV2: ~p", [_Event]),
    State.


%% @private
do_event(Event, State) ->
    nkdomain_obj_util:event(Event, State).
