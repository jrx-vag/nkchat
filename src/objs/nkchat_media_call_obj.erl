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

-module(nkchat_media_call_obj).
-behavior(nkdomain_obj).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([create/2, hangup/2, hangup_async/2]).
-export([add_member/4, remove_member/2, get_info/1]).
-export([invite/4, cancel_invite/1, accept_invite/5, reject_invite/1]).
-export([find_member_calls/2, find_calls_with_members/2]).
-export([send_candidate/3, set_status/3]).
-export([get_invite_token/1]).
-export([object_info/0, object_es_mapping/0, object_parse/2, object_create/1,
         object_api_syntax/2, object_api_cmd/2, object_send_event/2,
         object_init/1, object_save/1, object_sync_op/3, object_async_op/2,
         object_link_down/2, object_handle_info/2]).
-export([object_next_status_timer/1, object_check_active/1]).
-export([object_admin_info/0]).
-export_type([event/0]).

-include("nkchat.hrl").
-include_lib("nkdomain/include/nkdomain.hrl").
-include_lib("nkdomain/include/nkdomain_debug.hrl").
-include_lib("nkservice/include/nkservice.hrl").

-define(CHECK_DOWN_TIME, 5*60).     % Secs

-define(MAX_NEW_TIME, 300).
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


-type call_type() :: one2one.

-type call_status() :: new | in_call.

-type role() :: binary().

-type member_status() ::
    #{
        audio => boolean(),
        video => boolean(),
        screen => boolean()
    }.

-type create_opts() ::
    #{
        parent_id => nkdomain:id(),
        created_by => nkdomain:id(),
        obj_name => nkdomain:obj_name(),
        name => binary()
    }.


-type event() ::
    {status, call_status()} |
    {member_added, Member::nkdomain:obj_id(), SessId::nkdomain:obj_id(), [Role::role()]} |
    {member_removed, Member::nkdomain:obj_id(), [Role::role()]} |
    {member_down, Member::nkdomain:obj_id(), [Role::role()]} |
    {call_hangup, Reason::term()} |
    {new_candidate, #sdp_candidate{}} |     %% sdp_candidate.candidate = <<>> for end
    {member_status, Member::nkdomain:obj_id(), member_status()}.


%% ===================================================================
%% Public
%% ===================================================================


%% @doc
-spec create(nkdomain:id(), create_opts()) ->
    {ok, CallId::nkdomain:id(), pid()} | {error, term()}.

create(Domain, Opts) ->
    Base = maps:with([parent_id, created_by, obj_name, name], Opts),
    Obj =Base#{
        type => ?MEDIA_CALL,
        domain_id => Domain,
        active => true,
        ?MEDIA_CALL => #{
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


-spec add_member(nkdomain:id(), nkdomain:id(), nkdomain:obj_id(), [role()]) ->
    ok | {error, term()}.

add_member(Id, Member, SessId, Role) when is_binary(Role); is_atom(Role) ->
    add_member(Id, Member, SessId, [Role]);

add_member(Id, Member, SessId, Roles) when is_list(Roles) ->
    Roles2 = [nklib_util:to_binary(Role) || Role <- Roles],
    case nkdomain_lib:find(Member) of
        #obj_id_ext{type=?DOMAIN_USER, obj_id=MemberId} ->
            nkdomain_obj:sync_op(Id, {?MODULE, add_member, MemberId, SessId, self(), Roles2});
        #obj_id_ext{} ->
            {error, member_invalid};
        {error, object_not_found} ->
            {error, member_not_found};
        {error, Error} ->
            {error, Error}
    end.


%% @doc
-spec remove_member(nkdomain:id(), nkdomain:id()) ->
    ok | {error, term()}.

remove_member(Id, Member) ->
    case nkdomain_lib:find(Member) of
        #obj_id_ext{obj_id=MemberId} ->
            nkdomain_obj:sync_op(Id, {?MODULE, remove_member, MemberId});
        _ ->
            nkdomain_obj:sync_op(Id, {?MODULE, remove_member, Member})
    end.


%% @private
get_info(Pid) ->
    nkdomain_obj:sync_op(any, Pid, {?MODULE, get_info}).


%% @doc
-spec invite(nkdomain:id(), nkdomain:id(), nkdomain:id(), invite_opts()) ->
    {ok, InviteId::nkdomain:obj_id(), pid()} | {error, term()}.

invite(Id, Caller, Callee, InviteOpts) ->
    case nkdomain_lib:load(Caller) of
        #obj_id_ext{type = ?DOMAIN_USER, obj_id=CallerId} ->
            case nkdomain_lib:load(Callee) of
                #obj_id_ext{type = ?DOMAIN_USER, obj_id=CalleeId} ->
                    nkdomain_obj:sync_op(Id, {?MODULE, invite, CallerId, CalleeId, InviteOpts});
                {error, object_not_found} ->
                    {error, user_not_found};
                {error, Error} ->
                    {error, Error}
            end;
        {error, object_not_found} ->
            {error, user_not_found};
        {error, Error} ->
            {error, Error}
    end.


%% @doc
cancel_invite(InviteId) ->
    case consume_token(InviteId, cancelled) of
        {ok, CallId} ->
            nkdomain_obj:sync_op(CallId, {?MODULE, cancel_invite, InviteId});
        {error, Error} ->
            {error, Error}
    end.


%% @doc Accepts a invitation notification
-spec accept_invite(nkdomain:id(), nkdomain:id(), nkdomain:id(), [role()], accept_opts()) ->
    {ok, CallId::nkdomain:obj_id(), pid()} | {error, term()}.

accept_invite(InviteId, MemberId, SessId, Roles, AcceptOpts) ->
    case consume_token(InviteId, accepted) of
        {ok, CallId} ->
            case add_member(CallId, MemberId, SessId, Roles) of
                ok ->
                    nkdomain_obj:sync_op(CallId, {?MODULE, accept_invite, InviteId, AcceptOpts});
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.


%% @doc Rejects a invitation notification
-spec reject_invite(nkdomain:id()) ->
    ok | {error, term()}.

reject_invite(InviteId) ->
    case consume_token(InviteId, rejected) of
        {ok, CallId} ->
            nkdomain_obj:sync_op(CallId, {?MODULE, reject_invite, InviteId});
        {error, Error} ->
            {error, Error}
    end.


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

send_candidate(CallId, MemberId, Candidate) ->
    nkdomain_obj:sync_op(CallId, {?MODULE, send_candidate, MemberId, Candidate}).


%% @doc
-spec set_status(nkdomain:obj_id(), nkdomain:obj_id(), member_status()) ->
    ok | {error, term()}.

set_status(CallId, MemberId, Status) ->
    nkdomain_obj:sync_op(CallId, {?MODULE, set_status, MemberId, Status}).



%% =================================================================
%% nkdomain_obj behaviour
%% ===================================================================


-record(invite, {
    caller_id :: nkdomain_obj:id(),
    callee_id :: nkdomain_obj:id(),
    token_pid :: pid(),
    invite_opts :: invite_opts()
}).


-record(member, {
    added_time :: nkdomain:timestamp(),
    roles :: [role()],
    session_id :: nkdomain:obj_id(),
    session_pid :: pid(),
    status :: member_status()
}).


-record(session, {
    type :: call_type(),
    status :: call_status(),
    invites :: #{InviteId::nkdomain:obj_id() => #invite{}},
    members :: #{MemberId::nkdomain:obj_id() => #member{}}
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
        status => #{type => keyword},
        members_hash => #{type => keyword},
        members => #{
            type => object,
            dynamic => false,
            properties => #{
                member_id => #{type => keyword},
                added_time => #{type => date},
                roles => #{type => keyword}
            }
        }
    }.


%% @private
object_parse(update, _Obj) ->
    #{};

object_parse(_Mode, _Obj) ->
    #{
        type => {atom, [one2one]},
        status => {atom, [new]},
        members_hash => binary,
        members =>
            {list,
                 #{
                     member_id => binary,
                     added_time => integer,
                     roles => {list, binary},
                     '__mandatory' => [member_id, added_time, roles]
                 }
            },
        '__defaults' => #{type => one2one, members => []}
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
    #{type:=Type, status:=Status, ?MEDIA_CALL := #{members:=MemberList}} = Obj,
    Members = lists:map(
        fun(Data) ->
            #{
                member_id := MemberId,
                added_time := AddedTime,
                roles := Roles
            } = Data,
            {MemberId,
                #member{
                    added_time = AddedTime,
                    roles = Roles,
                    session_pid = undefined
                }}
        end,
        MemberList),
    Session = #session{
        type = Type,
        status = Status,
        invites = #{},
        members = maps:from_list(Members)
    },
    State2 = State#obj_state{session=Session},
    State3 = update_status(Status, State2),
    {ok, State3}.


%% @private Prepare the object for saving
object_save(#obj_state{obj=Obj, session=Session}=State) ->
    #session{type=Type, status=Status, members=Members} = Session,
    MemberList= lists:map(
        fun({MemberId, Member}) ->
            #member{
                added_time = Time,
                roles = Roles
            } = Member,
            #{
                member_id => MemberId,
                added_time => Time,
                roles => Roles
            }
        end,
        maps:to_list(Members)),
    #{?MEDIA_CALL:=Call1} = Obj,
    Call2 = Call1#{type=>Type, status=>Status, members=>MemberList},
    Obj2 = ?ADD_TO_OBJ(?MEDIA_CALL, Call2, Obj),
    {ok, State#obj_state{obj = Obj2}}.


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

object_sync_op({?MODULE, add_member, MemberId, SessId, Pid, Roles}, _From, State) ->
    case do_add_member(MemberId, SessId, Pid, Roles, State) of
        {ok, State2} ->
            {reply, ok, State2};
        {error, Error} ->
            {reply, {error, Error}, State}
    end;

object_sync_op({?MODULE, remove_member, MemberId}, _From, State) ->
    case do_remove_member(MemberId, State) of
        {ok, State2} ->
            {reply, ok, State2};
        {error, Error} ->
            {reply, {error, Error}, State}
    end;

object_sync_op({?MODULE, invite, CallerId, CalleeId, InviteOpts}, _From, State) ->
    Members = get_members(State),
    case maps:find(CallerId, Members) of
        {ok, _} ->
            case do_invite(CallerId, CalleeId, InviteOpts, State) of
                {ok, InviteId, InvitePid, State2} ->
                    {reply, {ok, InviteId, InvitePid}, State2};
                {error, Error} ->
                    {reply, {error, Error}, State}
            end;
        error ->
            {reply, {error, member_not_found}, State}
    end;

object_sync_op({?MODULE, cancel_invite, InviteId}, _From, State) ->
    State2 = rm_invite(InviteId, caller_cancelled, State),
    {reply, ok, State2};

object_sync_op({?MODULE, accept_invite, InviteId}, _From, State) ->
    Invites = get_invites(State),
    case maps:find(InviteId, Invites) of
        {ok, #invite{caller_id=CallerId, callee_id=CalleeId}} ->
            % Avoid user detecting the going down of token and send the invite_removed event
            State2 = rm_invite(InviteId, call_accepted, State),
            Members = get_members(State),
            case maps:find(CallerId, Members) of
                {ok, _} ->
                    case maps:find(CalleeId, Members) of
                        {ok, _} ->
                            #obj_state{id=#obj_id_ext{obj_id=CallId}} = State2,
                            State3 = update_status(in_call, State2),
                            {reply, {ok, CallId, self()}, State3};
                        error ->
                            {error, member_not_found, State}
                    end;
                error ->
                    {error, member_not_found, State}
            end;
        _ ->
            {reply, {error, token_invalid}, State}
    end;

object_sync_op({?MODULE, reject_invite, InviteId}, _From, State) ->
    State2 = rm_invite(InviteId, call_rejected, State),
    {reply, ok, State2};

object_sync_op({?MODULE, hangup_call, Reason}, _From, State) ->
    State2 = do_event_all_sessions({call_hangup, Reason}, State),
    {stop, normal, ok, State2};

object_sync_op({?MODULE, send_candidate, MemberId, Candidate}, _From, State) ->
    Members = get_members(State),
    Reply = case maps:find(MemberId, Members) of
        error ->
            {error, member_not_found};
        {ok, _Member} ->
            Members2 = maps:remove(MemberId, Members),
            case maps:to_list(Members2) of
                [{_, #member{session_pid=Pid}}] ->
                    do_event_session(Pid, {new_candidate, Candidate}, State),
                    ok;
                O ->
                    {error, {call_is_not_one2one, O}}
            end
    end,
    {reply, Reply, State};

object_sync_op({?MODULE, set_status, MemberId, Status}, _From, State) ->
    Members = get_members(State),
    case maps:find(MemberId, Members) of
        error ->
            {reply, {error, member_not_found}, State};
        {ok, #member{status=Status0}=Member} ->
            Member2 = Member#member{status=maps:merge(Status0, Status)},
            State2 = set_members(Members#{MemberId => Member2}, State),
            #obj_state{} = State2,
            State3 = do_event_all_sessions({member_status, MemberId, Status}, State2),
            {reply, ok, State3}
    end;

object_sync_op(_Op, _From, _State) ->
    continue.


%% @private
object_async_op({?MODULE, hangup_call, Reason}, State) ->
    State2 = do_event_all_sessions({call_hangup, Reason}, State),
    {stop, normal, State2};

object_async_op(_Op, _State) ->
    continue.


%% @private
object_link_down({usage, {?MODULE, session, MemberId, _SessId, _Pid}}, State) ->
    {ok, State2} = do_member_down(MemberId, State),
    {ok, State2};

object_link_down(_Link, State) ->
    {ok, State}.


%% @private
object_handle_info({?MODULE, member_down_check, MemberId}, State) ->
    Members = get_members(State),
    case maps:find(MemberId, Members) of
        {ok, #member{session_pid=undefined}} ->
            {ok, State2} = do_remove_member(MemberId, State),
            {noreply, State2};
        _ ->
            {noreply, State}
    end;

object_handle_info(_Info, _State) ->
    continue.

%% @doc
object_next_status_timer(#obj_state{session=#session{status=Status}}=State) ->
    State2 = case Status of
        new ->
            ?LLOG(debug, "too long in 'new' status", [], State),
            nkdomain_obj_util:set_next_status_timer(?MAX_NEW_TIME*1000, State)
    end,
    {ok, State2}.


%% @private
object_check_active(_Id) ->
    lager:notice("NKLOG CALL Checked ~p", [_Id]),
    force_load.


%% ===================================================================
%% Internal
%% ===================================================================

consume_token(InviteId, Reason) ->
    case nkdomain_token_obj:consume_token(InviteId, Reason) of
        {ok, Data} ->
            case get_invite_token(Data) of
                {ok, #{call_id:=CallId}} ->
                    {ok, CallId};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.


%% @doc
update_status(new, #obj_state{session=Session}=State) ->
    State2 = State#obj_state{session=Session#session{status=new}},
    State3 = nkdomain_obj_util:set_next_status_timer(?MAX_NEW_TIME*1000, State2),
    ?LLOG(info, "call is now in 'new' status", [], State),
    do_event({status, new}, State3).



%% @private
do_add_member(MemberId, SessId, Pid, Roles, State) ->
    Members = get_members(State),
    State2 = case maps:find(MemberId, Members) of
        {ok, #member{session_id=SessId, session_pid=Pid}} ->
            State;
        {ok, #member{session_id=OldSessId, session_pid=OldPid}} ->
            nkdomain_obj:links_remove(usage, {?MODULE, member, MemberId, OldSessId, OldPid}, State);
        error ->
            State
    end,
    Member = #member{
        roles = Roles,
        added_time = nkdomain_util:timestamp(),
        session_id = SessId,
        session_pid = Pid
    },
    Members2 = Members#{MemberId => Member},
    State2 = set_members(Members2, State),
    % We will detect session failures
    % We will be alive while the session is alive
    State3 = nkdomain_obj:links_add(usage, {?MODULE, member, MemberId, SessId, Pid}, State2),
    State4 = do_event_all_sessions({member_added, MemberId, SessId, Roles}, State3),
    {ok, State4}.


%% @private
do_remove_member(MemberId, State) ->
    Members = get_members(State),
    case maps:find(MemberId, Members) of
        {ok, #member{roles=Roles, session_id=SessId, session_pid=Pid}} ->
            State2 = do_event_all_sessions({member_removed, MemberId, Roles}, State),
            State3 = nkdomain_obj:links_remove(usage, {?MODULE, member, MemberId, SessId, Pid}, State2),
            Members2 = maps:remove(MemberId, Members),
            State4 = set_members(Members2, State3),
            case map_size(Members2) of
                0 ->
                    hangup_async(self(), no_members);
                _ ->
                    ok
            end,
            {ok, State4};
        error ->
            {error, member_not_found}
    end.


%% @private
do_member_down(MemberId, State) ->
    Members = get_members(State),
    case maps:find(MemberId, Members) of
        {ok, #member{roles=Roles, session_id=SessId, session_pid=Pid}=Member} ->
            State2 = do_event_all_sessions({member_down, MemberId, Roles}, State),
            erlang:send_after(1000*?CHECK_DOWN_TIME, self(), {?MODULE, member_down_check, MemberId}),
            Member2 = Member#member{session_pid=undefined},
            Members2 = Members#{MemberId => Member2},
            State3 = set_members(Members2, State2),
            State4 = nkdomain_obj:links_remove(usage, {?MODULE, member, MemberId, SessId, Pid}, State3),
            {ok, State4};
        error ->
            {ok, State}
    end.


%% @private
do_invite(CallerId, CalleeId, InviteOpts, State) ->
    #obj_state{domain_id=DomainId, parent_id=CallerId, id=#obj_id_ext{srv_id=SrvId}} = State,
    Op1 = make_invite_token(CallerId, CalleeId, InviteOpts, State),
    Push = make_invite_push(CallerId, InviteOpts),
    Opts = #{srv_id=>SrvId, wakeup_push => Push},
    case nkdomain_user_obj:add_token_notification(CalleeId, ?MEDIA_SESSION, Opts, Op1) of
        {ok, _MemberId, Op2} ->
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
            case nkdomain_token_obj:create(DomainId, TokenOpts, Op2) of
                {ok, InviteId, Pid, _Secs} ->
                    State2 = add_invite(InviteId, Pid, CallerId, CalleeId, InviteOpts, State),
                    {ok, InviteId, Pid, State2};
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
        invite_id => <<>>,
        full_name => FullName,
        audio => maps:get(audio, InviteOpts, false),
        video => maps:get(video, InviteOpts, false),
        screen => maps:get(screen, InviteOpts, false),
        conversation_id => maps:get(conversation_id, InviteOpts, <<>>)
    }.


%% @private
get_members(#obj_state{session=Session}) ->
    #session{members=Members} = Session,
    Members.


%% @private
set_members(Members, #obj_state{session=Session}=State) ->
    Session2 = Session#session{members=Members},
    set_members_hash(State#obj_state{session=Session2, is_dirty=true}).


%% @private
do_event(Event, State) ->
    nkdomain_obj_util:event(Event, State).


%% @private
do_event_all_sessions(Event, State) ->
    Members = get_members(State),
    State2 = do_event_all_sessions(maps:to_list(Members), Event, State),
    do_event(Event, State2).


%% @private
do_event_all_sessions([], _Event, State) ->
    State;

do_event_all_sessions([{_MemberId, #member{session_pid=Pid}}|Rest], Event, #obj_state{}=State) ->
    do_event_session(Pid, Event, State),
    do_event_all_sessions(Rest, Event, State).


%% @private
do_event_session(Pid, Event, #obj_state{id=#obj_id_ext{obj_id=CallId}}) ->
    nkchat_media_session_obj:call_event(Pid, CallId, Event).


%% @private
set_members_hash(#obj_state{obj=Obj, session=#session{members=Members}}=State) ->
    MemberIds = maps:keys(Members),
    Hash = get_members_hash(MemberIds),
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
    #session{members=Members} = Session,
    lists:map(
        fun({MemberId, Member}) ->
            #member{
                added_time = Time,
                roles = Roles,
                session_pid = Pid
            } = Member,
            #{
                member_id => MemberId,
                added_time => Time,
                roles => Roles,
                active => is_pid(Pid)
            }
        end,
        maps:to_list(Members)).

%% @private
get_invites(#obj_state{session=Session}) ->
    #session{invites=Invites} = Session,
    Invites.


%% @private
add_invite(InviteId, Pid, CallerId, CalleeId, InviteOpts, #obj_state{session=Session}=State) ->
    Invites = get_invites(State),
    Invite = #invite{
        caller_id = CallerId,
        callee_id = CalleeId,
        token_pid = Pid,
        invite_opts = InviteOpts
    },
    Invites2 = Invites#{InviteId => Invite},
    Session2 = Session#session{invites=Invites2},
    State2 = State#obj_state{session=Session2},
    nkdomain_obj:links_add(usage, {?MODULE, invite, InviteId, Pid}, State2).


%% @private
rm_invite(InviteId, Reason, #obj_state{session=Session}=State) ->
    Invites = get_invites(State),
    case maps:take(InviteId, Invites) of
        {#invite{callee_id=CalleeId, token_pid=TokenPid}, Invites2} ->
            % Avoid user detecting the going down of token
            nkdomain_user_obj:remove_token_notification(CalleeId, InviteId, Reason),
            Session2 = Session#session{invites=Invites2},
            State2 = do_event({invite_removed, InviteId, Reason}, State),
            State3 = State2#obj_state{session=Session2},
            nkdomain_obj:links_remove(usage, {?MODULE, invite, InviteId, TokenPid}, State3);
        error ->
            State
    end.
