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

%% @doc Call Object
%%
%% One2One Calls
%% -------------
%%
%% - You must first create the call.
%%     - It will have no members (and set a timeout to stop if none is added)
%% - You register a session as caller, and send an invite
%%     - Invite is a token that is sent to the callee
%%     - Call enters "ringing" state
%%     - If the caller cancels, the caller session stops, or the call is hangup, the token is removed
%% - The callee can accept the invite, adding a callee session
%%     - The answer is sent to the caller
%%     - The call enters the "in call" state















-module(nkchat_media_call_obj).
-behavior(nkdomain_obj).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([create/2, hangup/2, hangup_async/2]).
-export([add_session/3, remove_session/2, get_info/1]).
-export([invite/5, stop_media/3, answer_media/5]).
-export([find_member_calls/2, find_calls_with_members/2]).
-export([send_candidate/3, set_status/3]).
-export([get_invite_token/1, heartbeat/1]).
-export([object_info/0, object_es_mapping/0, object_parse/2, object_create/1,
         object_api_syntax/2, object_api_cmd/2, object_send_event/2,
         object_init/1, object_save/1, object_stop/2, object_sync_op/3, object_async_op/2,
         object_handle_info/2]).
-export([object_next_status_timer/1, object_check_active/1]).
-export([object_admin_info/0]).
-export_type([event/0]).

-include("nkchat.hrl").
-include_lib("nkdomain/include/nkdomain.hrl").
-include_lib("nkdomain/include/nkdomain_debug.hrl").
-include_lib("nkservice/include/nkservice.hrl").

-define(CHECK_DOWN_TIME, 5*60).     % Secs

-define(MAX_NEW_TIME, 300).
-define(MAX_RINGING_TIME, 300).
-define(MAX_CALL_TIME, 4*60*60).
-define(DEFAULT_INVITE_TTL, 3*60).     % Secs


%% ===================================================================
%% Types
%% ===================================================================


-type invite_opts() ::
#{
    sdp => binary(),
    trickle_ice => boolean(),
    ttl => integer(),
    audio => boolean(),
    video => boolean(),
    screen => boolean(),
    conversation_id => binary()
}.

-type accept_opts() ::
#{
    sdp => binary(),
    trickle_ice => boolean(),
    audio => boolean,
    video => boolean,
    screen => boolean
}.


-type call_type() :: direct | one2one | room.

-type call_status() :: new | ringing | in_call.

-type role() :: binary().

-type session_status() ::
    #{
        audio => boolean(),
        video => boolean(),
        screen => boolean()
    }.

-type create_opts() ::
    #{
        type => call_type(),
        parent_id => nkdomain:id(),
        created_by => nkdomain:id(),
        obj_name => nkdomain:obj_name(),
        name => binary()
    }.


-type event() ::
    {status, call_status()} |
    {session_added, SessId::nkdomain:obj_id(), User::nkdomain:obj_id(), #{}} |
    {session_removed, SessId::nkdomain:obj_id(), User::nkdomain:obj_id(), #{}} |
    {session_status, SessId::nkdomain:obj_id(), session_status()} |
    {media_started, MediaId::nkdomain:obj_id()} |
    {media_stopped, MediaId::nkdomain:obj_id(), Reason::term()} |
    {call_hangup, Reason::term()} |
    {new_candidate, SessId::nkdomain:obj_id(), #sdp_candidate{}}.     %% sdp_candidate.candidate = <<>> for end


%% ===================================================================
%% Public
%% ===================================================================


%% @doc
-spec create(nkdomain:id(), create_opts()) ->
    {ok, CallId::nkdomain:id(), pid()} | {error, term()}.

create(Domain, #{type:=_}=Opts) ->
    Base = maps:with([parent_id, created_by, obj_name, name], Opts),
    Obj =Base#{
        type => ?MEDIA_CALL,
        domain_id => Domain,
        active => true,
        ?MEDIA_CALL => #{
            type => maps:get(type, Opts)
        }
    },
    case nkdomain_obj_make:create(Obj) of
        {ok, #obj_id_ext{obj_id=CallId, pid=Pid}, _} ->
            {ok, CallId, Pid};
        {error, Error} ->
            {error, Error}
    end.


%% @doc
-spec hangup(nkdomain:id(), nkservice:error()) ->
    ok | {error, term()}.

hangup(Id, Reason) ->
    nkdomain_obj:sync_op(Id, {?MODULE, hangup_call, Reason}).


%% @doc
-spec hangup_async(nkdomain:id(), nkservice:error()) ->
    ok | {error, term()}.

hangup_async(Id, Reason) ->
    nkdomain_obj:async_op(Id, {?MODULE, hangup_call, Reason}).


%% @doc Registers an user session
-spec add_session(nkdomain:id(), nkdomain:obj_id(), nkdomain:obj_id()) ->
    ok | {error, term()}.

add_session(Id, SessId, UserId) ->
    nkdomain_obj:sync_op(Id, {?MODULE, add_session, SessId, self(), UserId}).


%% @doc
-spec remove_session(nkdomain:id(), nkdomain:obj_id()) ->
    ok | {error, term()}.

remove_session(Id, SessId) ->
    nkdomain_obj:sync_op(Id, {?MODULE, remove_session, SessId}).


%% @private
get_info(Pid) ->
    nkdomain_obj:sync_op(any, Pid, {?MODULE, get_info}).


%% @doc Valid for direct calls, adds the sessions and starts the invite
-spec invite(nkdomain:id(), nkdomain:obj_id(), nkdomain:obj_id(), nkdomain:id(), invite_opts()) ->
    {ok, InviteId::nkdomain:obj_id()} | {error, term()}.

invite(Id, SessId, UserId, Callee, InviteOpts) ->
    case nkdomain_lib:load(Callee) of
        #obj_id_ext{type = ?DOMAIN_USER, obj_id=CalleeId} ->
            nkdomain_obj:sync_op(Id, {?MODULE, invite, SessId, self(), UserId, CalleeId, InviteOpts});
        {error, object_not_found} ->
            {error, user_not_found};
        {error, Error} ->
            {error, Error}
    end.


%% @doc Accepts a invitation notification in a direct call
-spec answer_media(nkdomain:id(), nkdomain:obj_id(), nkdoman:obj_id(), nkdomain:id(), accept_opts()) ->
    {ok, CallId::nkdomain:obj_id(), pid()} | {error, term()}.

answer_media(CallId, MediaId, SessId, UserId, AcceptOpts) ->
    nkdomain_obj:sync_op(CallId, {?MODULE, answer_media, MediaId, SessId, self(), UserId, AcceptOpts}).


%% @doc
stop_media(CallId, MediaId, Reason) ->
    nkdomain_obj:async_op(CallId, {?MODULE, stop_media, MediaId, Reason}).


%% @doc
heartbeat(CallId) ->
    nkdomain_obj:async_op(CallId, {?MODULE, heartbeat}).


%% @doc
find_member_calls(Domain, MemberId) ->
    case nkdomain_lib:find(Domain) of
        #obj_id_ext{type=?DOMAIN_DOMAIN, obj_id=DomainId} ->
            Filters = #{
                type => ?MEDIA_CALL,
                domain_id => DomainId,
                << ?MEDIA_CALL/binary, ".members.member_id">> => MemberId
            },
            Search2 = #{
                fields => [<<?MEDIA_CALL/binary, ".type">>],
                filters => Filters,
                size => 9999
            },
            case nkdomain:search(Search2) of
                {ok, _N, List, _Meta} ->
                    List2 = lists:map(
                        fun(#{<<"obj_id">>:=CallId, ?MEDIA_CALL:=#{<<"type">>:=Type}}) -> {CallId, Type} end,
                        List),
                    {ok, List2};
                {error, Error} ->
                    {error, Error}
            end;
        _ ->
            {error, domain_unknown}
    end.


%% @doc
find_calls_with_members(Domain, MemberIds) ->
    case nkdomain_lib:find(Domain) of
        #obj_id_ext{type=?DOMAIN_DOMAIN, obj_id=DomainId} ->
            Hash = get_members_hash(MemberIds),
            Filters = #{
                type => ?MEDIA_CALL,
                domain_id => DomainId,
                << ?MEDIA_CALL/binary, ".members_hash">> => Hash
            },
            Search2 = #{
                fields => [<<?MEDIA_CALL/binary, ".type">>],
                filters => Filters,
                size => 9999
            },
            case nkdomain:search(Search2) of
                {ok, _N, List, _Meta} ->
                    List2 = lists:map(
                        fun(#{<<"obj_id">>:=CallId, ?MEDIA_CALL:=#{<<"type">>:=Type}}) -> {CallId, Type} end,
                        List),
                    {ok, List2};
                {error, Error} ->
                    {error, Error}
            end;
        _ ->
            {error, domain_unknown}
    end.


%% @doc
-spec send_candidate(nkdomain:obj_id(), nkdomain:obj_id(), #sdp_candidate{}) ->
    ok | {error, term()}.

send_candidate(CallId, SessId, Candidate) ->
    nkdomain_obj:sync_op(CallId, {?MODULE, send_candidate, SessId, Candidate}).


%% @doc
-spec set_status(nkdomain:obj_id(), nkdomain:obj_id(), session_status()) ->
    ok | {error, term()}.

set_status(CallId, SessId, Status) ->
    nkdomain_obj:sync_op(CallId, {?MODULE, set_status, SessId, Status}).



%% =================================================================
%% nkdomain_obj behaviour
%% ===================================================================


-record(session_member, {
    session_id :: nkdomain_obj:id(),
    user_id :: nkdomain:obj_id(),
    session_pid :: pid(),
    session_mon :: reference()
}).


-record(session_media, {
    type :: direct,
    status :: ringing | answered,
    media_id :: nkdomain_obj:id(),
    timer :: reference(),
    caller_session_id = <<>> :: nkdomain_obj:id(),
    caller_opts = #{} :: invite_opts(),
    caller_status = #{} :: session_status(),
    callee_session_id = <<>> :: nkdomain_obj:id(),     % For other than 'direct' would be the MS
    callee_opts = #{} :: invite_opts(),
    callee_status = #{} :: session_status()
}).



-record(session, {
    type :: call_type(),
    status :: call_status(),
    roles :: #{MemberId::nkdomain:obj_id() => [role()]},
    members2 :: [#session_member{}],
    medias2 :: [#session_media{}]
}).



%% @private
object_info() ->
    #{
        type => ?MEDIA_CALL,
        stop_after_disabled => true,
        remove_after_stop => true
    }.

%% @doc
object_admin_info() ->
    #{
        class => session,
        weight => 2000,
        tree_id => <<"domain_tree_sessions_media.calls">>
    }.


%% @private
object_es_mapping() ->
    #{
        type => #{type => keyword},
        members_hash => #{type => keyword},
        members => #{
            type => object,
            dynamic => false,
            properties => #{
                member_id => #{type => keyword}
            }
        }
    }.


%% @private
object_parse(update, _Obj) ->
    #{};

object_parse(_Mode, _Obj) ->
    #{
        type => {atom, [direct, one2one, room]},
        members_hash => binary,
        members =>
            {list,
                 #{
                     member_id => binary,
                     '__mandatory' => [member_id]
                 }
            },
        '__defaults' => #{type => direct, members => []}
    }.


%% @doc
object_create(Obj) ->
    #{?MEDIA_CALL := Call} = Obj,
    Obj2 = Obj#{?MEDIA_CALL => Call#{members => []}},
    nkdomain_obj_make:create(Obj2).


%% @private
object_send_event(Event, State) ->
    nkchat_media_call_events:event(Event, State).


%% @private
object_api_syntax(Cmd, Syntax) ->
    nkchat_media_call_obj_syntax:syntax(Cmd, Syntax).


%% @private
object_api_cmd(Cmd, Req) ->
    nkchat_media_call_obj_cmd:cmd(Cmd, Req).


%% @private
%% Client must start a session or it will be destroyed after timeout
object_init(#obj_state{obj=Obj}=State) ->
    #{?MEDIA_CALL := #{type:=Type}} = Obj,
    Session = #session{
        type = Type,
        roles = #{},
        members2 = [],
        medias2 = []
    },
    State2 = State#obj_state{session=Session},
    State3 = update_status(new, State2),
    {ok, State3}.


%% @private Prepare the object for saving
object_save(#obj_state{obj=Obj, session=Session}=State) ->
    #session{type=Type, status=Status, members2=Members} = Session,
    MemberList= lists:map(
        fun(#session_member{user_id = UserId}) ->
            #{
                member_id => UserId
            }
        end,
        Members),
    #{?MEDIA_CALL:=Call1} = Obj,
    Call2 = Call1#{type=>Type, status=>Status, members=>MemberList},
    Obj2 = ?ADD_TO_OBJ(?MEDIA_CALL, Call2, Obj),
    {ok, State#obj_state{obj = Obj2}}.


%% @private
object_stop(_Reason, State) ->
    {ok, do_hangup(object_stopped, State)}.


%% @private
object_sync_op({?MODULE, get_info}, _From, State) ->
    #obj_state{obj=Obj, session=Session} = State,
    #session{type=Type} = Session,
    Data = #{
        name => maps:get(name, Obj, <<>>),
        description => maps:get(description, Obj, <<>>),
        type => Type,
        members => expand_members(State)
    },
    {reply, {ok, Data}, State};


object_sync_op({?MODULE, add_session, SessId, Pid, UserId}, _From, State) ->
    State2 = add_session_member(SessId, Pid, UserId, State),
    {reply, ok, State2};

object_sync_op({?MODULE, remove_session, SessId}, _From, State) ->
    case rm_session_member(SessId, State) of
        {ok, State2} ->
            {reply, ok, State2};
        {error, Error} ->
            {reply, {error, Error}, State}
    end;

object_sync_op({?MODULE, invite, SessId, Pid, UserId, CalleeId, InviteOpts}, _From, State) ->
    case get_type_status(State) of
        {direct, new} ->
            case State of
                #obj_state{session=#session{members2=[], medias2=[]}} ->
                    State2 = add_session_member(SessId, Pid, UserId, State),
                    case do_invite(UserId, SessId, CalleeId, InviteOpts, State2) of
                        {ok, InviteId, State3} ->
                            State4 = update_status(ringing, State3),
                            {reply, {ok, InviteId}, State4};
                        {error, Error} ->
                            {reply, {error, Error}, State2}
                    end;
                _ ->
                    {reply, {error, call_status_invalid}, State}
            end;
        _ ->
            {reply, {error, call_status_invalid}, State}
    end;


object_sync_op({?MODULE, answer_media, MediaId, SessId, SessPid, UserId, AcceptOpts}, _From, State) ->
    case get_session_media(MediaId, State) of
        #session_media{status=ringing, caller_session_id=CallerSessId, timer=Timer}=Media ->
            nklib_util:cancel_timer(Timer),
            State2 = add_session_member(SessId, SessPid, UserId, State),
            Media2 = Media#session_media{
                status = answered,
                timer = undefined,
                callee_session_id = SessId,
                callee_opts = AcceptOpts,
                caller_status = #{}
            },
            State3 = update_session_media(Media2, State2),
            State4 = do_session_member_event(CallerSessId, {media_answered, MediaId, AcceptOpts}, State3),
            State5 = do_session_member_event(SessId, {media_started, MediaId}, State4),
            State6 = update_status(in_call, State5),
            #obj_state{id=#obj_id_ext{obj_id=CallId}} = State6,
            {reply, {ok, CallId, self()}, State6};
        #session_media{} ->
            {reply, {error, session_already_answered}, State};
        not_found ->
            {reply, {error, session_not_found}, State}
    end;

object_sync_op({?MODULE, hangup_call, Reason}, _From, State) ->
    State2 = do_hangup(Reason, State),
    {stop, normal, ok, State2};

object_sync_op({?MODULE, send_candidate, SessId, Candidate}, _From, State) ->
    case get_session_media_session(SessId, State) of
        {caller, #session_media{callee_session_id=Remote}} ->
            do_session_member_event(Remote, {new_candidate, Remote, Candidate}, State),
            {reply, ok, State};
        {callee, #session_media{caller_session_id=Remote}} ->
            do_session_member_event(Remote, {new_candidate, Remote, Candidate}, State),
            {reply, ok, State};
        not_found ->
            {error, session_not_found}
    end;

object_sync_op({?MODULE, set_status, SessId, Status}, _From, State) ->
    case get_session_media_session(SessId, State) of
         {caller, #session_media{caller_status=OldStatus, callee_session_id=Remote}=Media} ->
            NewStatus = maps:merge(OldStatus, Status),
            Media2 = Media#session_media{caller_status=NewStatus},
             State2 = update_session_media(Media2, State),
            do_session_member_event(Remote, {member_status, SessId, Status}, State),
            {ok, State2};
        {callee, #session_media{callee_status=OldStatus, caller_session_id=Remote}=Media} ->
            NewStatus = maps:merge(OldStatus, Status),
            Media2 = Media#session_media{callee_status=NewStatus},
            State2 = update_session_media(Media2, State),
            do_session_member_event(Remote, {member_status, SessId, Status}, State),
            {ok, State2};
        not_found ->
            {error, session_not_found}
    end;

object_sync_op(_Op, _From, _State) ->
    continue.


%% @private
object_async_op({?MODULE, heartbeat}, State) ->
    State2 = case get_type_status(State) of
        {direct, in_call} ->
            update_status(in_call, State);
        _ ->
            State

    end,
    {noreply, State2};

object_async_op({?MODULE, stop_media, MediaId, Reason}, State) ->
    State2 = case get_session_media(MediaId, State) of
        #session_media{}=Media ->
            rm_session_media(Media, Reason, State);
        not_found ->
            State
    end,
    {noreply, State2};

object_async_op({?MODULE, hangup_call, Reason}, State) ->
    State2 = do_hangup(Reason, State),
    {stop, normal, State2};

object_async_op(_Op, _State) ->
    continue.


%% @private
object_handle_info({?MODULE, ringing_timeout, MediaId}, State) ->
    case get_session_media(MediaId, State) of
        #session_media{status=ringing}=Media ->
            State2 = rm_session_media(Media, ring_timeout, State),
            {noreply, State2};
        _ ->
            ?LLOG(warning, "received ringing_timeout for unknown media", [], State),
            {noreply, State}
    end;

object_handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    #obj_state{session=#session{members2=Members}} = State,
    case lists:keyfind(Pid, #session_member.session_pid, Members) of
        #session_member{session_id=SessId} ->
            State2 = rm_session_member(SessId, State),
            {noreply, State2};
        false ->
            continue
    end;

object_handle_info(_Info, _State) ->
    continue.


%% @doc
object_next_status_timer(#obj_state{session=#session{status=Status}}=State) ->
    ?LLOG(notice, "too long in '~s' status", [Status], State),
    hangup_async(self(), timeout),
    {ok, State}.


%% @private
object_check_active(_Id) ->
    lager:notice("NKLOG CALL Checked ~p", [_Id]),
    force_load.


%% ===================================================================
%% Internal
%% ===================================================================

%% @private
get_type_status(#obj_state{session=#session{type=Type, status=Status}}) ->
    {Type, Status}.


%%%% @private
%%consume_token(InviteId, Reason) ->
%%    case nkdomain_token_obj:consume_token(InviteId, Reason) of
%%        {ok, Data} ->
%%            case get_invite_token(Data) of
%%                {ok, #{call_id:=CallId}} ->
%%                    {ok, CallId};
%%                {error, Error} ->
%%                    {error, Error}
%%            end;
%%        {error, Error} ->
%%            {error, Error}
%%    end.


%% @doc
update_status(hangup, #obj_state{session=Session}=State) ->
    State#obj_state{session=Session#session{status=hangup}};

update_status(Status, #obj_state{session=Session}=State) ->
    State2 = State#obj_state{session=Session#session{status=Status}},
    Time = case Status of
        new ->
            ?MAX_NEW_TIME;
        ringing ->
            ?MAX_RINGING_TIME;
        in_call ->
            ?MAX_CALL_TIME
    end,
    ?LLOG(info, "call is now in '~s' status", [Status], State),
    State3 = nkdomain_obj_util:set_next_status_timer(Time*1000, State2),
    do_all_session_members_event({status, Status}, State3).


%% @private
get_session_member(SessId, #obj_state{session=#session{members2=Members}}) ->
    case lists:keyfind(SessId, #session_member.session_id, Members) of
        #session_member{}=Member ->
            Member;
        false ->
            not_found
    end.


%% @private
put_session_member(SessId, Member, #obj_state{session=#session{members2=Members} = Session} = State) ->
    Members2 = lists:keystore(SessId, #session_member.session_id, Members, Member),
    Session2 = Session#session{members2=Members2},
    State#obj_state{session=Session2}.


%% @private
add_session_member(SessId, Pid, UserId, State) ->
    case get_session_member(SessId, State) of
        #session_member{user_id=UserId, session_pid=Pid} ->
            State;
        not_found ->
            Member = #session_member{
                user_id = UserId,
                session_id = SessId,
                session_pid = Pid,
                session_mon = monitor(process, Pid)
            },
            State2 = put_session_member(SessId, Member, State),
            State3 = do_all_session_members_event({session_added, SessId, UserId, #{}}, State2),
            set_members_hash(State3)
    end.


%% @private
rm_session_member(SessId, #obj_state{session=Session} = State) ->
    #session{members2=Members, medias2=Medias} = Session,
    case lists:keytake(SessId, #session_member.session_id, Members) of
        {value, #session_member{user_id=UserId, session_pid=Pid}, Members2} ->
            demonitor(Pid),
            Session2 = Session#session{members2=Members2},
            State2 = set_members_hash(State#obj_state{session=Session2}),
            State3 = do_all_session_members_event({session_removed, UserId, #{}}, State2),
            Medias = lists:filter(
                fun(#session_media{caller_session_id=Caller, callee_session_id=Callee}) ->
                    Caller==SessId orelse Callee==SessId
                end,
                Medias),
            % TODO: move to 'recover'
            State4 = rm_session_medias(Medias, member_stopped, State3),
            {ok, State4};
        false ->
            {error, session_not_found}
    end.


%% @private
get_session_media(MediaId, #obj_state{session=#session{medias2=Medias}}) ->
    case lists:keyfind(MediaId, #session_media.media_id, Medias) of
        #session_media{}=Media ->
            Media;
        false ->
            not_found
    end.


%% @private
get_session_media_session(SessId, #obj_state{session=#session{medias2=Medias}}) ->
    case lists:keyfind(SessId, #session_media.caller_session_id, Medias) of
        #session_media{} = Media ->
            {caller, Media};
        false ->
            case lists:keyfind(SessId, #session_media.callee_session_id, Medias) of
                #session_media{} = Media ->
                    {callee, Media};
                false ->
                    not_found
            end
    end.


%% @private
add_session_media(#session_media{media_id=MediaId} = Media, State) ->
    State2 = update_session_media(Media, State),
    do_event({media_started, MediaId}, State2).


%% @private
update_session_media(#session_media{media_id=MediaId} = Media, #obj_state{session=Session} = State) ->
    #session{medias2=Medias} = Session,
    Medias2 = lists:keystore(MediaId, #session_media.media_id, Medias, Media),
    Session2 = Session#session{medias2=Medias2},
    State#obj_state{session=Session2}.


%% @private Removes a series of media sessions. If no one is remaining, stop
rm_session_medias([], _Reason, State) ->
    State;

rm_session_medias([#session_media{} = Media|Rest], Reason, State) ->
    State2 = rm_session_media(Media, Reason, State),
    rm_session_media(Rest, Reason, State2).


%% @private
rm_session_media(Media, Reason, #obj_state{session=Session} = State) ->
    #session_media{
        media_id = MediaId,
        caller_session_id = Caller,
        callee_session_id = Callee,
        timer = Timer
    } = Media,
    nklib_util:cancel_timer(Timer),
    #session{medias2=Medias} = Session,
    do_session_member_event(Caller, {media_stopped, MediaId, Reason}, State),
    do_session_member_event(Callee, {media_stopped, MediaId, Reason}, State),
    Medias2 = lists:keydelete(MediaId, #session_media.media_id, Medias),
    case Medias2 of
        [] ->
            case get_type_status(State) of
                {direct, _} when Medias==[] ->
                    hangup_async(self(), no_remaining_medias);
                _ ->
                    ok
            end;
        _ ->
            ok
    end,
    Session2 = Session#session{medias2=Medias2},
    State#obj_state{session=Session2}.



%% @private
do_invite(CallerId, SessId, CalleeId, InviteOpts, State) ->
    #obj_state{domain_id=DomainId, id=#obj_id_ext{srv_id=SrvId}} = State,
    TokenData1 = make_invite_token(CallerId, CalleeId, InviteOpts, State),
    Push = make_invite_push(CallerId, InviteOpts),
    Opts = #{srv_id=>SrvId, wakeup_push => Push},
    case nkdomain_user_obj:add_token_notification(CalleeId, ?MEDIA_SESSION, Opts, TokenData1) of
        {ok, _MemberId, TokenData2} ->
            TTL = case InviteOpts of
                #{ttl:=TTL0} when is_integer(TTL0), TTL0>0 ->
                    TTL0;
                _ ->
                    ?DEFAULT_INVITE_TTL
            end,
            TokenOpts = #{
                parent_id => CalleeId,
                created_by => CalleeId,
                subtype => <<"media.call">>,
                ttl => TTL
            },
            case nkdomain_token_obj:create(DomainId, TokenOpts, TokenData2) of
                {ok, MediaId, _Pid, _Secs} ->
                    Timer = erlang:send_after(self(), 1100*TTL, {?MODULE, ringing_timeout, MediaId}),
                    Media = #session_media{
                        type = direct,
                        status = ringing,
                        media_id = MediaId,
                        timer = Timer,
                        caller_session_id = SessId,
                        caller_opts = InviteOpts
                    },
                    State2 = add_session_media(Media, State),
                    State3 = do_session_member_event(SessId, {media_ringing_out, MediaId}, State2),
                    {ok, MediaId, State3};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.


%% @private
make_invite_token(CallerId, CalleeId, InviteOpts, #obj_state{id=#obj_id_ext{obj_id=CallId}}) ->
    #{
        ?MEDIA_CALL =>
            #{
                <<"invite_op">> =>
                    #{
                        <<"call_id">> => CallId,
                        <<"caller_id">> => CallerId,
                        <<"callee_id">> => CalleeId,
                        <<"invite_opts">> => InviteOpts
                    }
            }
    }.


%% @private
get_invite_token(Token) ->
    case Token of
        #{
            ?MEDIA_CALL :=
            #{
                <<"invite_op">> :=
                #{
                    <<"call_id">> := CallId,
                    <<"caller_id">> := CallerId,
                    <<"callee_id">> := CalleeId,
                    <<"invite_opts">> := InviteOpts
                }
            }
        } ->
            Data = #{
                call_id => CallId,
                caller_id => CallerId,
                callee_id => CalleeId,
                invite_opts => InviteOpts
            },
            {ok, Data};
        _ ->
            {error, invalid_token}
    end.


%% @private
make_invite_push(CallerId, InviteOpts) ->
    {ok, #{fullname:=FullName}} = nkdomain_user_obj:get_name(CallerId),
    #{
        type => ?MEDIA_CALL,
        class => invite,
        media_id => <<>>,
        full_name => FullName,
        audio => maps:get(audio, InviteOpts, false),
        video => maps:get(video, InviteOpts, false),
        screen => maps:get(screen, InviteOpts, false),
        conversation_id => maps:get(conversation_id, InviteOpts, <<>>)
    }.


%% @private
do_hangup(Reason, State) ->
    case get_type_status(State) of
        {_, hangup} ->
            State;
        _ ->
            #obj_state{session=#session{medias2=Medias}} = State,
            State2 = rm_session_medias(Medias, call_hangup, State),
            State3 = do_all_session_members_event({call_hangup, Reason}, State2),
            update_status(hangup, State3)
    end.


%% @private
do_session_member_event(Pid, Event, #obj_state{id=#obj_id_ext{obj_id=CallId}}) when is_pid(Pid) ->
    nkchat_media_session_obj:call_event(Pid, CallId, Event);

do_session_member_event(SessId, Event, State) ->
    case get_session_member(SessId, State) of
        #session_member{session_pid=Pid} when is_pid(Pid) ->
            do_session_member_event(Pid, Event, State);
        _ ->
            ok
    end.


%% @private
do_all_session_members_event(Event, State) ->
    #obj_state{session=#session{members2=Members}} = State,
    State2 = do_all_session_members_event(Members, Event, State),
    do_event(Event, State2).


%% @private
do_all_session_members_event([], _Event, State) ->
    State;

do_all_session_members_event([#session_member{session_pid=Pid}|Rest], Event, #obj_state{} = State) ->
    do_session_member_event(Pid, Event, State),
    do_all_session_members_event(Rest, Event, State).


%% @private
do_event(Event, State) ->
    nkdomain_obj_util:event(Event, State).


%% @private
set_members_hash(#obj_state{obj=Obj, session=#session{members2=Members}}=State) ->
    UserIds = [UserId || #session_member{user_id=UserId} <- Members],
    Hash = get_members_hash(UserIds),
    #{?MEDIA_CALL:=Call} = Obj,
    Call2 = Call#{members_hash=>Hash},
    Obj2 = ?ADD_TO_OBJ(?MEDIA_CALL, Call2, Obj),
    State#obj_state{obj=Obj2, is_dirty=true}.


%% @private
get_members_hash(MemberIds) ->
    MemberIds2 = lists:usort(MemberIds),
    base64:encode(crypto:hash(sha, erlang:term_to_binary(MemberIds2))).


%% @doc
expand_members(#obj_state{session=Session}) ->
    #session{members2=Members} = Session,
    lists:map(
        fun(#session_member{user_id = MemberId}) ->
            #{
                member_id => MemberId
            }
        end,
        Members).
