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

-export([create/5, hangup_call/3, hangup_call_async/3]).
-export([add_member/6, remove_member/3, get_member_info/3, get_info/1]).
-export([find_member_calls/3, find_calls_with_members/3]).
%%-export([add_invite_op/3]).
-export([object_info/0, object_es_mapping/0, object_parse/3, object_create/2,
         object_api_syntax/2, object_api_cmd/2, object_send_event/2,
         object_init/1, object_save/1, object_sync_op/3, object_async_op/2,
         object_event/2, object_link_down/2, object_handle_info/2]).
-export([object_admin_info/0]).
-export_type([event/0]).

-include("nkchat.hrl").
-include_lib("nkdomain/include/nkdomain.hrl").
-include_lib("nkdomain/include/nkdomain_debug.hrl").
-include_lib("nkservice/include/nkservice.hrl").

-define(CHECK_DOWN_TIME, 5*60).     % Secs

%% ===================================================================
%% Types
%% ===================================================================


-type call_type() :: one2one.

-type role() :: binary().

-type call_opts() ::
    #{
        name => binary(),
        sdp => binary(),
        trickle_ice => boolean(),
        ttl => integer()
    }.


-type event() ::
    {member_added, Member::nkdomain:obj_id(), [Role::role()], SessId::nkdomain:obj_id(), pid()} |
    {member_removed, Member::nkdomain:obj_id(), [Role::role()], SessId::nkdomain:obj_id()} |
    {member_down, Member::nkdomain:obj_id(), [Role::role()]} |
    {call_hangup, Reason::term()}.

%% ===================================================================
%% Public
%% ===================================================================


%% @doc
create(SrvId, Domain, Name, User, Type) ->
    Obj = #{
        type => ?MEDIA_CALL,
        domain_id => Domain,
        created_by => User,
        name => Name,
        active => true,
        ?MEDIA_CALL => #{
            type => Type
        }
    },
    case nkdomain_obj_make:create(SrvId, Obj) of
        {ok, #obj_id_ext{obj_id=CallId, pid=Pid}, _} ->
            {ok, CallId, Pid};
        {error, Error} ->
            {error, Error}
    end.



%% @doc
-spec hangup_call(nkservice:id(), nkdomain:id(), nkservice:error()) ->
    ok | {error, term()}.

hangup_call(SrvId, Id, Reason) ->
    nkdomain_obj:sync_op(SrvId, Id, {?MODULE, hangup_call, Reason}).


%% @doc
-spec hangup_call_async(nkservice:id(), nkdomain:id(), nkservice:error()) ->
    ok | {error, term()}.

hangup_call_async(SrvId, Id, Reason) ->
    nkdomain_obj:async_op(SrvId, Id, {?MODULE, hangup_call, Reason}).


-spec add_member(nkservice:id(), nkdomain:id(), nkdomain:id(), [role()], nkdomain:obj_id(), call_opts()) ->
    ok | {error, term()}.

add_member(SrvId, Id, Member, Role, SessId, CallOpts) when is_binary(Role); is_atom(Role) ->
    add_member(SrvId, Id, Member, [nklib_util:to_binary(Role)], SessId, CallOpts);

add_member(SrvId, Id, Member, Roles, SessId, CallOpts) when is_list(Roles) ->
    case nkdomain_lib:find(SrvId, Member) of
        #obj_id_ext{type = ?DOMAIN_USER, obj_id=MemberId} ->
            nkdomain_obj:sync_op(SrvId, Id, {?MODULE, add_member, MemberId, Roles, SessId, self(), CallOpts});
        #obj_id_ext{} ->
            {error, member_invalid};
        {error, object_not_found} ->
            {error, member_not_found};
        {error, Error} ->
            {error, Error}
    end.


%% @doc
-spec remove_member(nkservice:id(), nkdomain:id(), nkdomain:id()) ->
    ok | {error, term()}.

remove_member(SrvId, Id, Member) ->
    case nkdomain_lib:find(SrvId, Member) of
        #obj_id_ext{obj_id=MemberId} ->
            nkdomain_obj:sync_op(SrvId, Id, {?MODULE, remove_member, MemberId});
        _ ->
            nkdomain_obj:sync_op(SrvId, Id, {?MODULE, remove_member, Member})
    end.


%% @private
-spec get_member_info(nkservice:id(), nkdomain:obj_id(), nkdomain:obj_id()) ->
    {ok, map()} | {error, term()}.

get_member_info(SrvId, CallId, MemberId) ->
    nkdomain_obj:sync_op(SrvId, CallId, {?MODULE, get_member_info, MemberId}).


%% @private
get_info(Pid) ->
    nkdomain_obj:sync_op(any, Pid, {?MODULE, get_info}).

%% @doc
find_member_calls(SrvId, Domain, MemberId) ->
    case nkdomain_lib:find(SrvId, Domain) of
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
            case nkdomain:search(SrvId, Search2) of
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
find_calls_with_members(SrvId, Domain, MemberIds) ->
    case nkdomain_lib:find(SrvId, Domain) of
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
            case nkdomain:search(SrvId, Search2) of
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


%%%% @doc
%%add_invite_op(SrvId, CallId, Callee, Base) ->
%%    case nkdomain_lib:find(SrvId, Callee) of
%%        #obj_id_ext{obj_id=CalleeId} ->
%%            nkdomain_obj:sync_op(SrvId, CallId, {?MODULE, add_invite_op, CalleeId, Base});
%%        {error, Error} ->
%%            {error, Error}
%%    end.


%%%% @doc
%%perform_op(SrvId, #{?MEDIA_CALL:=#{<<"add_member_op">>:=Op}}) ->
%%    #{
%%        <<"conversation_id">> := CallId,
%%        <<"member_id">> := MemberId,
%%        <<"user_id">> := _UserId
%%    } = Op,
%%    case add_member(SrvId, CallId, MemberId) of
%%        {ok, _MemberId} ->
%%            ok;
%%        {error, Error} ->
%%            {error, Error}
%%    end;
%%
%%perform_op(_SrvId, _Data) ->
%%    {error, operation_token_invalid}.


%% =================================================================
%% nkdomain_obj behaviour
%% ===================================================================

-record(member, {
    added_time :: nkdomain:timestamp(),
    roles :: [role()],
    session_id :: nkdomain:obj_id(),
    session_pid :: pid(),
    call_opts :: call_opts()
}).

-record(session, {
    type :: call_type(),
    caller_id :: nkdomain:obj_id(),
    callee_id :: nkdomain:obj_id(),
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
object_parse(_SrvId, update, _Obj) ->
    #{};

object_parse(_SrvId, _Mode, _Obj) ->
    #{
        type => {atom_or_binary, [one2one]},
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
object_create(SrvId, Obj) ->
    #{?MEDIA_CALL := Call} = Obj,
    Obj2 = Obj#{?MEDIA_CALL => Call#{members => []}},
    nkdomain_obj_make:create(SrvId, Obj2).


%% @private
object_send_event(Event, State) ->
    nkchat_conversation_obj_events:event(Event, State).


%% @private
object_api_syntax(Cmd, Syntax) ->
    nkchat_conversation_obj_syntax:api(Cmd, Syntax).


%% @private
object_api_cmd(Cmd, Req) ->
    nkchat_conversation_obj_api:cmd(Cmd, Req).


%% @private
%% Client must start a session or it will be destroyed after timeout
object_init(#?STATE{obj=Obj}=State) ->
    #{?MEDIA_CALL := #{members:=MemberList, type:=Type}} = Obj,
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
        members = maps:from_list(Members)
    },
    {ok, State#?STATE{session=Session}}.


%% @private Prepare the object for saving
object_save(#?STATE{obj=Obj, session=Session}=State) ->
    #session{members=Members} = Session,
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
    Call2 = Call1#{members=>MemberList},
    Obj2 = ?ADD_TO_OBJ(?MEDIA_CALL, Call2, Obj),
    {ok, State#?STATE{obj = Obj2}}.


%% @private
object_sync_op({?MODULE, get_info}, _From, State) ->
    #?STATE{obj=#{?MEDIA_CALL:=ChatCall}=Obj} = State,
    #{type:=Type} = ChatCall,
    Data = #{
        name => maps:get(name, Obj, <<>>),
        description => maps:get(description, Obj, <<>>),
        type => Type,
        members => expand_members(State)
    },
    {reply, {ok, Data}, State};

object_sync_op({?MODULE, add_member, MemberId, Roles, SessId, Pid, CallOpts}, _From, State) ->
    case do_add_member(MemberId, Roles, SessId, Pid, CallOpts, State) of
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

object_sync_op({?MODULE, add_invite_op, CalleeId, Base}, _From, State) ->
    #?STATE{session=Session} = State,
    case Session of
        #session{type=one2one} ->
            case do_one2one_invite(CalleeId, Base, State) of
                {ok, Op} ->
                    {reply, {ok, CalleeId, Op}, State};
                {error, Error} ->
                    {reply, {error, Error}, State}
            end;
        _ ->
            {reply, {error, operation_invalid}, State}
    end;

object_sync_op({?MODULE, hangup_call, Reason}, _From, State) ->
    State2 = do_event({call_hangup, Reason}, State),
    {stop, Reason, ok, State2};

object_sync_op(_Op, _From, _State) ->
    continue.


%% @private
object_async_op({?MODULE, hangup_call, Reason}, State) ->
    State2 = do_event({call_hangup, Reason}, State),
    {stop, Reason, State2};

object_async_op(_Op, _State) ->
    continue.


%% @private
object_link_down({usage, {?MODULE, session, MemberId, _SessId, _Pid}}, State) ->
    {ok, State2} = do_member_down(MemberId, State),
    {ok, State2};

object_link_down(_Link, State) ->
    {ok, State}.


%% @private
object_event({member_added, MemberId, Roles, _SessId, _Pid}, State) ->
    {ok, do_event_all_sessions({member_added, MemberId, Roles}, State)};

object_event({member_removed, MemberId, Roles, _SessId}, State) ->
    {ok, do_event_all_sessions({member_removed, MemberId, Roles}, State)};

object_event({member_down, MemberId, Roles}, State) ->
    {ok, do_event_all_sessions({member_down, MemberId, Roles}, State)};

object_event({call_hangup, Reason}, State) ->
    {ok, do_event_all_sessions({call_hangup, Reason}, State)};

object_event(_Event, State) ->
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


%% ===================================================================
%% Internal
%% ===================================================================

do_one2one_invite(_CalleeId, Base, State) ->
    #?STATE{id=#obj_id_ext{obj_id=CallId}} = State,
    Members = get_members(State),
    case maps:to_list(Members) of
        [{CallerId, #member{call_opts=#{sdp:=SDP}}}] ->
            Op = Base#{
                ?MEDIA_CALL => #{
                    <<"invite_op">> => #{
                        <<"call_id">> => CallId,
                        <<"caller_id">> =>  CallerId,
                        <<"sdp">> => SDP
                    }
                }
            },
            {ok, Op};
        _ ->
            {error, operation_invalid}
    end.


%% @private
do_add_member(MemberId, Roles, SessId, Pid, CallOpts, State) ->
    Members = get_members(State),
    case maps:find(MemberId, Members) of
        error ->
            Member = #member{
                roles = Roles,
                added_time = nkdomain_util:timestamp(),
                session_id = SessId,
                session_pid = Pid,
                call_opts = CallOpts
            },
            Members2 = Members#{MemberId => Member},
            State2 = set_members(Members2, State),
            State3 = nkdomain_obj:links_add(usage, {?MODULE, member, MemberId, SessId, Pid}, State2),
            State4 = do_event({member_added, MemberId, Roles, SessId, Pid}, State3),
            {ok, State4};
        {ok, _} ->
            {error, member_already_present}
    end.


%% @private
do_remove_member(MemberId, State) ->
    Members = get_members(State),
    case maps:find(MemberId, Members) of
        {ok, #member{roles=Roles, session_id=SessId, session_pid=Pid}} ->
            State2 = do_event({member_removed, MemberId, Roles}, State),
            State3 = nkdomain_obj:links_remove(usage, {?MODULE, member, MemberId, SessId, Pid}, State2),
            Members2 = maps:remove(MemberId, Members),
            State4 = set_members(Members2, State3),
            case map_size(Members2) of
                0 ->
                    hangup_call_async(any, self(), no_members);
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
            State2 = do_event({member_down, MemberId, Roles}, State),
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
get_members(#?STATE{session=Session}) ->
    #session{members=Members} = Session,
    Members.


%% @private
set_members(Members, #?STATE{session=Session}=State) ->
    Session2 = Session#session{members=Members},
    set_members_hash(State#?STATE{session=Session2, is_dirty=true}).


%% @private
do_event(Event, State) ->
    nkdomain_obj_util:event(Event, State).


%% @private
do_event_all_sessions(Event, #?STATE{id=#obj_id_ext{obj_id=CallId}}=State) ->
    Members = get_members(State),
    do_event_all_sessions(CallId, maps:to_list(Members), Event, State),
    do_event(Event, State).


%% @private
do_event_all_sessions(_CallId, [], _Event, State) ->
    State;

do_event_all_sessions(CallId, [{_MemberId, #member{session_pid=Pid}}|Rest], Event, State) ->
    nkchat_media_session_obj:call_event(Pid, CallId, Event),
    do_event_all_sessions(CallId, Rest, Event, State).


%% @private
set_members_hash(#?STATE{obj=Obj, session=#session{members=Members}}=State) ->
    MemberIds = maps:keys(Members),
    Hash = get_members_hash(MemberIds),
    #{?MEDIA_CALL:=Call} = Obj,
    Call2 = Call#{members_hash=>Hash},
    Obj2 = ?ADD_TO_OBJ(?MEDIA_CALL, Call2, Obj),
    State#?STATE{obj=Obj2, is_dirty=true}.


%% @private
get_members_hash(MemberIds) ->
    MemberIds2 = lists:usort(MemberIds),
    base64:encode(crypto:hash(sha, erlang:term_to_binary(MemberIds2))).


%% @doc
expand_members(#?STATE{session=Session}) ->
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
