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
-export([invite/4, accept_call/4, reject_call/3]).
-export([object_info/0, object_es_mapping/0, object_parse/3,
         object_api_syntax/2, object_api_cmd/2]).
-export([object_init/1, object_stop/2, object_send_event/2,
         object_sync_op/3, object_async_op/2]).
-export([object_admin_info/0]).
-export([call_event/3, notify_fun/5]).

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
    {conversation_activated, CallId::nkdomain:obj_id()} |
    {conversation_added, CallId::nkdomain:obj_id()} |
    {conversation_removed, CallId::nkdomain:obj_id()} |
    {member_added, CallId::nkdomain:obj_id(), IsActive::boolean(), MemberId::nkdomain:obj_id()} |
    {member_removed, CallId::nkdomain:obj_id(), IsActive::boolean(), MemberId::nkdomain:obj_id()} |
    {message_created, nkdomain:obj()} |
    {message_updated, nkdomain:obj()} |
    {message_deleted, nkdomain:obj_id()} |
    {unread_counter_updated, CallId::nkdomain:obj_id(), integer()} |
    {invited_to_conversation, TokenId::binary(), UserId::binary(), CallId::binary()} |
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
-spec get_calls(nkservice:id(), nkdomain:id()) ->
    {ok, [nkdomain:obj_id()]} | {error, term()}.

get_calls(SrvId, Id) ->
    nkdomain_obj:sync_op(SrvId, Id, {?MODULE, get_calls}).


%% @doc
-spec get_call_info(nkservice:id(), nkdomain:id(), nkdomain:id()) ->
    {ok, map()} | {error, term()}.

get_call_info(SrvId, Id, Call) ->
    case nkdomain_lib:find(SrvId, Call) of
        #obj_id_ext{obj_id=CallId} ->
            nkdomain_obj:sync_op(SrvId, Id, {?MODULE, get_call_info, CallId});
        {error, Error} ->
            {error, Error}
    end.


%% @doc Accepts a invitation notification
-spec accept_call(nkservice:id(), nkdomain:id(), nkdomain:id(), nkchat_media_call_obj:call_opts()) ->
    ok | {error, term()}.

accept_call(SrvId, SessId, CallId, CallOpts) ->
    case nkdomain_token_obj:consume_token(SrvId, CallId) of
        {ok, #{data:=Data}} ->
            nkdomain_obj:sync_op(SrvId, SessId, {?MODULE, accept_call, Data, CallOpts});
        {error, Error} ->
            {error, Error}
    end.


%% @doc Rejects a invitation notification
-spec reject_call(nkservice:id(), nkdomain:id(), nkdomain:id()) ->
    ok | {error, term()}.

reject_call(SrvId, _SessId, CallId) ->
    case nkdomain_token_obj:consume_token(SrvId, CallId) of
        {ok, _Data} ->
            lager:error("NKLOG Token consumed"),
            ok;
        {error, Error} ->
            {error, Error}
    end.


%% @private To be called from nkdomain_user_obj
call_event(Pid, CallId, Event) ->
    nkdomain_obj:async_op(any, Pid, {?MODULE, call_event, CallId, Event}).


%% @private To be called from nkdomain_user_obj
notify_fun(_SessId, Pid, TokenId, Msg, Op) ->
    % lager:error("NKLOG SESS FUN ~p ~p", [Op, Msg]),
    nkdomain_obj:async_op(any, Pid, {?MODULE, notify, TokenId, Msg, Op}).



%% ===================================================================
%% nkdomain_obj behaviour
%% ===================================================================

-record(call, {
    pid :: pid(),
    caller_id :: nkdomain:obj_id(),
    caller_sdp :: binary(),
    caller_opts :: nkchat_media_call_obj:call_opts(),
    callee_id :: nkdomain:obj_id(),
    callee_sdp :: binary(),
    callee_opts :: nkchat_media_call_obj:call_opts()
}).


-record(session, {
    user_id :: nkdomain:obj_id(),
    calls :: #{nkdomain:obj_id() => #call{}}
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
    #session{calls=Calls} = Session,
    {reply, {ok, maps:keys(Calls)}, State};

object_sync_op({?MODULE, invite, CalleeId, CallOpts}, _From, State) ->
    #?STATE{srv_id=SrvId, domain_id=DomainId, parent_id=CallerId, id=#obj_id_ext{obj_id=SessId}} = State,
    Name = maps:get(name, CallOpts, <<>>),
    case nkchat_media_call_obj:create(SrvId, DomainId, Name, CallerId, one2one) of
        {ok, CallId, CallPid} ->
            case nkchat_media_call_obj:add_member(any, CallPid, CallerId, caller, SessId, CallOpts) of
                ok ->
                    case nkchat_media_call_obj:invite(any, CallPid, CalleeId) of
                        {ok, TokenId} ->
                            {reply, {ok, CallId, TokenId}, State};
                        {error, Error} ->
                            nkchat_media_call_obj:hangup_call_async(SrvId, CallPid, caller_hangup),
                            {reply, {error, Error}, State}
                    end;
                {error, Error} ->
                    nkchat_media_call_obj:hangup_call_async(SrvId, CallPid, caller_hangup),
                    {reply, {error, Error}, State}
            end;
        {error, Error} ->
            {reply, {error, Error}, State}
    end;

object_sync_op({?MODULE, accept_call, Data}, _From, #?STATE{srv_id=SrvId}=State) ->
    Reply = nkchat_conversation_obj:perform_op(SrvId, Data),
    {reply, Reply, State};

object_sync_op(_Op, _From, _State) ->
    continue.


%% @private
object_async_op({?MODULE, call_event, CallId, Event}, State) ->
    State2 = do_call_event(Event, CallId, State),
    {noreply, State2};

object_async_op({?MODULE, notify, TokenId, Msg, Op}, State) ->
    case Msg of
        #{
            ?MEDIA_SESSION := #{
                <<"invited_to_conversation_op">> := #{
                    <<"conversation_id">> := CallId,
                    <<"user_id">> := UserId,
                    <<"member_id">> := _MemberId
                }
            }
        } ->
            Event = case Op of
                created ->
                    {invited_to_conversation, TokenId, UserId, CallId};
                removed ->
                    {remove_notification, TokenId}
            end,
            {noreply, do_event(Event, State)};
        _ ->
            ?LLOG(warning, "unxpected notify: ~p", [Msg], State),
            {noreply, State}
    end;

object_async_op(_Op, _State) ->
    continue.



%% ===================================================================
%% Internal
%% ===================================================================



%%%% @private
%%do_add_conv(CallId, State) ->
%%    #?STATE{srv_id=SrvId, id=#obj_id_ext{obj_id=SessId}} = State,
%%    #?STATE{session=Session} = State,
%%    #session{user_id=UserId, conv_pids=Calls1} = Session,
%%    case nkchat_conversation_obj:add_session(SrvId, CallId, UserId, SessId, #{}) of
%%        {ok, Pid} ->
%%            monitor(process, Pid),
%%            Calls2 = Calls1#{CallId => Pid},
%%            Session2 = Session#session{conv_pids=Calls2},
%%            {ok, State#?STATE{session=Session2}};
%%        {error, Error} ->
%%            {error, Error}
%%    end.
%%
%%
%%%% @private
%%do_rm_conv(CallId, State) ->
%%    case get_conv_pid(CallId, State) of
%%        {ok, Pid} ->
%%            #?STATE{id=#obj_id_ext{obj_id=SessId}} = State,
%%            #?STATE{session=Session} = State,
%%            #session{user_id=UserId, conv_pids=CallPids1, active_id=ActiveId} = Session,
%%            nkchat_conversation_obj:remove_session(any, Pid, UserId, SessId),
%%            CallPids2 = maps:remove(Pid, CallPids1),
%%            Session2 = Session#session{conv_pids=CallPids2},
%%            Session3 = case ActiveId of
%%                CallId ->
%%                    Session2#session{active_id=undefined};
%%                _ ->
%%                    Session2
%%            end,
%%            State2 = do_event({conversation_removed, CallId}, State),
%%            {ok, State2#?STATE{session=Session3}};
%%        not_found ->
%%            {error, conversation_not_found}
%%    end.


%% @private
%%do_call_event({member_added, MemberId}, CallId, State) ->
%%    do_event({member_added, CallId, MemberId}, State);
%%
%%do_call_event({member_removed, MemberId}, CallId, #?STATE{session=Session}=State) ->
%%    State2 = case Session of
%%        #session{user_id=MemberId} ->
%%            {ok, S} = do_rm_conv(CallId, State),
%%            S;
%%        _ ->
%%            State
%%    end,
%%    do_event({member_removed, CallId, MemberId}, State2);
%%
%%do_call_event({message_created, Msg}, CallId, State) ->
%%    do_event({message_created, CallId, Msg}, State);
%%
%%do_call_event({message_updated, Msg}, CallId, State) ->
%%    do_event({message_updated, CallId, Msg}, State);
%%
%%do_call_event({message_deleted, MsgId}, CallId, State) ->
%%    do_event({message_deleted, CallId, MsgId}, State);
%%
%%do_call_event({counter_updated, Counter}, CallId, State) ->
%%    do_event({unread_counter_updated, CallId, Counter}, State);

do_call_event(_Event, _CallId, State) ->
    lager:error("SESS EV2"),
    State.


%% @private
do_event(Event, State) ->
    nkdomain_obj_util:event(Event, State).
