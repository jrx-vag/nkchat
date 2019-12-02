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

%% @doc Conversation Object
%% Messages having this conversation call message_event/2, and this object
%% will send events message_created, deleted, updated.
%%
%% You can add and remove members (and stores member creation date)
%% You can attach sessions to existing members, with any metadata
%% All sessions events are forwarded to the session in object_event/2

-module(nkchat_conversation_obj).
-behavior(nkdomain_obj).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([object_info/0, object_es_mapping/0, object_db_get_query/3, object_parse/2, object_create/1,
         object_api_syntax/2, object_api_cmd/2, object_send_event/2,
         object_init/1, object_save/1, object_sync_op/3, object_async_op/2,
         object_event/2, object_link_down/2, object_handle_info/2]).
-export([object_execute/5, object_schema/1, object_query/3, object_mutation/3]).
-export([object_admin_info/0]).
-export([make_members_hash/1]).
-export_type([event/0, query/0]).

-include("nkchat.hrl").
-include_lib("nkdomain/include/nkdomain.hrl").
-include_lib("nkdomain/include/nkdomain_debug.hrl").
-include_lib("nkservice/include/nkservice.hrl").

-define(MSG_CACHE_SIZE, 25).
-define(INVITE_TTL, 3*24*60*60).



%% ===================================================================
%% Types
%% ===================================================================



-type event() ::
    {added_info, nkchat_conversation:info()} |
    {last_seen_message, MemberIds::[nkdomain:obj()], Time::binary()} |
    {message_created, nkdomain:obj()} |
    {message_updated, nkdomain:obj()} |
    {message_deleted, nkdomain:obj_id()} |
    {member_added, nkdomain:obj_id(), MemberData::map()} |
    {added_to_conversation, nkdomain:obj_id()} |        % Same but obj_id is for the member
    {conversation_loaded, nkdomain:obj_id()} |          % Same but obj_id is for the member
    {member_muted, nkdomain:obj_id(), boolean()} |
    {member_removed, nkdomain:obj_id()} |
    {removed_from_conversation, nkdomain:obj_id()} |    % Same but obj_id is for the member
    {member_typing, nkdomain:obj_id()} |
    {session_added, Member::nkdomain:obj_id(), SessId::nkdomain:obj_id()} |
    {session_removed, Member::nkdomain:obj_id(), SessId::nkdomain:obj_id()} |
    {status_updated, nkchat_conversation:status()} |
    {is_closed_updated, boolean()}.



%% =================================================================
%% nkdomain_obj behaviour
%% ===================================================================

-record(chat_session, {
    session_id :: nkdomain:obj_id(),
    meta :: map(),
    pid :: pid(),
    is_active :: boolean()
}).

-record(member, {
    member_id :: nkdomain:obj_id(),
    added_time :: nkdomain:timestamp(),
    roles :: [nkchat_conversation:member_role()],
    last_active_time = 0 :: nkdomain:timestamp(),
    last_seen_msg_time = 0 :: nkdomain:timestamp(),
    unread_count = -1 :: integer(),
    sessions = [] :: [#chat_session{}]
}).

-record(invitation, {
    token_id :: nkdomain:obj_id(),
    pid :: pid(),
    invited_by :: nkdomain:obj_id(),
    user_id :: nkdomain:obj_id(),
    created_time = 0 :: nkdomain:timestamp(),
    expires_time :: nkdomain:timestamp()
}).

-record(session, {
    name :: binary(),
    type :: binary(),
    members :: [#member{}],
    invitations :: [#invitation{}],
    status :: nkchat_conversation:status(),
    is_closed :: boolean(),
    total_messages :: integer(),
    % sent_invitations = #{} :: #{pid() => nkdomain:obj_id()},
    messages :: [{Time::integer(), MsgId::nkdomain:obj_id(), Msg::map()}],
    last_message_time = 0 :: nkdomain:timestamp(),
    obj_name_follows_members :: boolean(),
    push_srv_id :: binary()
}).


%% @private
object_info() ->
    #{
        type => ?CHAT_CONVERSATION,
        schema_type => 'ChatConversation',
        default_ttl => 5*60*1000,
        dont_create_childs_on_disabled => true,
        dont_update_on_disabled => true
    }.

%% @doc
object_admin_info() ->
    #{
        class => resource,
        weight => 2000,
        type_view_mod => nkchat_conversation_obj_type_view,
        obj_view_mod => nkchat_conversation_obj_view
    }.



%% @private
object_parse(update, _Obj) ->
    #{};

object_parse(_Mode, _Obj) ->
    #{
        type => binary,
        status => binary,
        is_closed => boolean,                          % No new members or messages
        members =>
            {list,
                 #{
                     member_id => binary,
                     added_time => integer,
                     member_roles => {list, binary},
                     last_active_time => integer,
                     last_seen_message_time => integer,
                     '__mandatory' => [member_id, added_time, last_active_time, last_seen_message_time]
                 }
            },
        info => {list, map},
        invitations =>
            {list,
                #{
                    token_id => binary,
                    invited_by => binary,
                    user_id => binary,
                    created_time => integer,
                    expires_time => integer
                }},
        members_hash => binary,
        obj_name_follows_members => boolean,
        push_srv_id => binary,
        last_message_time => integer,
        initial_member_ids => {list, binary},
        '__defaults' => #{type => <<"private">>, info => [], members => [], invitations=>[]}
    }.




%% @private
object_es_mapping() ->
    #{
        type => #{type => keyword},
        status => #{type => keyword},
        is_closed => #{type => boolean},
        members => #{
            type => object,
            dynamic => false,
            properties => #{
                member_id => #{type => keyword},
                added_time => #{type => date},
                member_roles => #{type => keyword},
                last_active_time => #{type => date},
                last_seen_message_time => #{type => date}
            }
        },
        invitations => #{
            type => object,
            dynamic => false,
            properties => #{
                token_id => #{type => keyword},
                invited_by => #{type => keyword},
                user_id => #{type => keyword},
                created_time => #{type => date},
                expires_time => #{type => date}
            }
        },
        info => #{enabled => false},
        members_hash => #{type => keyword},
        obj_name_follows_members => #{type => boolean},
        push_srv_id => #{type => keyword},
        last_message_time => #{type => date}
    }.


%% @doc
object_schema(Type) ->
    nkchat_conversation_obj_schema:object_schema(Type).


%% @doc
object_execute(Field, ObjIdExt, #{?CHAT_CONVERSATION:=Conv}, Args, Ctx) ->
    nkchat_conversation_obj_schema:object_execute(Field, ObjIdExt, Conv, Args, Ctx).


%% @doc
object_query(QueryName, Params, Ctx) ->
    nkchat_conversation_obj_schema:object_query(QueryName, Params, Ctx).


%% @doc
object_mutation(MutationName, Params, Ctx) ->
    nkchat_conversation_obj_schema:object_mutation(MutationName, Params, Ctx).


-type query() ::
    {query_member_conversations, nkdomain:id(), nkdomain:id()} |
    {query_recent_conversations, nkdomain:id(), nkdomain:id(), #{from=>integer(),
                                    size=>integer(), types=>[binary()], omitted_types=>[binary()], unread=>boolean()}} |
    {query_conversations, nkdomain:id(), #{from=>integer(), size=>integer(),
                                    types=>[binary()], omitted_types=>[binary()]}} |
    {query_conversations_with_members, nkdomain:id(), [nkdomain:obj_id()]} |
    {query_conversation_messages, nkdomain:id(), nkdomain_db:search_objs_opts() |
                                    #{start_date=>nkdomain:timestamp(), end_date=>nkdomain:timestamp(), inclusive=>boolean()}}.


%% @doc
object_db_get_query(nkelastic, {query_member_conversations, Domain, Member}, DbOpts) ->
    case nkdomain_store_es_util:get_path(Domain) of
        {ok, DomainPath} ->
            case nkdomain_store_es_util:get_obj_id(Member) of
                {ok, MemberId} ->
                    Filters = [
                        {path, subdir, DomainPath},
                        {[?CHAT_CONVERSATION, ".members.member_id"], eq, MemberId}
                    ],
                    Opts = #{
                        type => ?CHAT_CONVERSATION,
                        size => 9999,
                        fields => [list_to_binary([?CHAT_CONVERSATION, ".type"])],
                        sort => [#{<<"updated_time">> => #{order => desc}},
                                 #{<<"created_time">> => #{order => desc}}]
                    },
                    {ok, {nkelastic, Filters, maps:merge(DbOpts, Opts)}};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end;

object_db_get_query(nkelastic, {query_recent_conversations, Domain, Member, Opts}, DbOpts) ->
    case nkdomain_store_es_util:get_path(Domain) of
        {ok, DomainPath} ->
            case nkdomain_store_es_util:get_obj_id(Member) of
                {ok, MemberId} ->
                    Filters = case Opts of
                        #{types := Types} when is_list(Types) ->
                            [{[?CHAT_CONVERSATION, ".type"], values, Types}];
                        _ ->
                            []
                    end,
                    Filters1 = case Opts of
                        #{omitted_types := OmittedTypes} when is_list(OmittedTypes) ->
                            [{'not', {[?CHAT_CONVERSATION, ".type"], values, OmittedTypes}}|Filters];
                        _ ->
                            Filters
                    end,
                    Filters2 = [
                        {path, subdir, DomainPath},
                        {[?CHAT_CONVERSATION, ".members.member_id"], eq, MemberId}
                    |Filters1],
                    Fields = case Opts of
                        #{unread := true} ->
                            [list_to_binary([?CHAT_CONVERSATION, ".members"]),
                             list_to_binary([?CHAT_CONVERSATION, ".last_message_time"])];
                        _ ->
                            []
                    end,
                    Opts2 = maps:with([from, size], Opts),
                    Opts3 = Opts2#{
                        type => ?CHAT_CONVERSATION,
                        fields => [list_to_binary([?CHAT_CONVERSATION, ".type"])|Fields],
                        sort => [#{list_to_binary([?CHAT_CONVERSATION, ".last_message_time"]) => #{order => desc}}]
                    },
                    {ok, {nkelastic, Filters2, maps:merge(DbOpts, Opts3)}};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end;

object_db_get_query(nkelastic, {query_conversations, Domain, Opts}, DbOpts) ->
    case nkdomain_store_es_util:get_path(Domain) of
        {ok, DomainPath} ->
            Filters = case Opts of
                #{types := Types} when is_list(Types) ->
                    [{[?CHAT_CONVERSATION, ".type"], values, Types}];
                _ ->
                    []
            end,
            Filters1 = case Opts of
                #{omitted_types := OmittedTypes} when is_list(OmittedTypes) ->
                    [{'not', {[?CHAT_CONVERSATION, ".type"], values, OmittedTypes}}|Filters];
                _ ->
                    Filters
            end,
            Filters2 = [
                {path, subdir, DomainPath}
            |Filters1],
            Fields = case Opts of
                #{unread := true} ->
                    [list_to_binary([?CHAT_CONVERSATION, ".members"]),
                     list_to_binary([?CHAT_CONVERSATION, ".last_message_time"])];
                _ ->
                    []
            end,
            Opts2 = maps:with([from, size], Opts),
            Opts3 = Opts2#{
                type => ?CHAT_CONVERSATION,
                fields => [list_to_binary([?CHAT_CONVERSATION, ".type"])|Fields],
                sort => [#{list_to_binary([?CHAT_CONVERSATION, ".last_message_time"]) => #{order => desc}}]
            },
            {ok, {nkelastic, Filters2, maps:merge(DbOpts, Opts3)}};
        {error, Error} ->
            {error, Error}
    end;

object_db_get_query(nkelastic, {query_conversations_with_members, Domain, MemberIds}, DbOpts) ->
    case nkdomain_store_es_util:get_obj_id(Domain) of
        {ok, DomainId} ->
            Hash = nkchat_conversation_obj:make_members_hash(MemberIds),
            Filters = [
                {domain_id, eq, DomainId},
                {[?CHAT_CONVERSATION, ".members_hash"], eq, Hash}
            ],
            Opts = #{
                type => ?CHAT_CONVERSATION,
                size=>9999,
                fields => [list_to_binary([?CHAT_CONVERSATION, ".type"])]
            },
            {ok, {nkelastic, Filters, maps:merge(DbOpts, Opts)}};
        {error, Error} ->
            {error, Error}
    end;

object_db_get_query(nkelastic, {query_conversation_messages, Conv, Opts}, DbOpts) ->
    case nkdomain_store_es_util:get_obj_id(Conv) of
        {ok, ConvId} ->
            {Order, Filters1} = case Opts of
                #{start_date:=Date1, end_date:=Date2, inclusive:=true} ->
                    {desc, [{created_time, gte, Date1}, {created_time, lte, Date2}]};
                #{start_date:=Date1, end_date:=Date2} ->
                    {desc, [{created_time, gt, Date1}, {created_time, lt, Date2}]};
                #{start_date:=Date, inclusive:=true} ->
                    {asc, [{created_time, gte, Date}]};
                #{start_date:=Date} ->
                    {asc, [{created_time, gt, Date}]};
                #{end_date:=Date, inclusive:=true} ->
                    {desc, [{created_time, lte, Date}]};
                #{end_date:=Date} ->
                    {desc, [{created_time, lt, Date}]};
                _ ->
                    {desc, []}
            end,
            Filters2 = [
                {parent_id, eq, ConvId}
                | Filters1
            ],
            Opts2 = maps:with([from, size], Opts),
            Opts3 = Opts2#{
                type => ?CHAT_MESSAGE,
                sort => [#{created_time => #{order => Order}}],
                fields => [created_time, created_by, ?CHAT_MESSAGE]
            },
            {ok, {nkelastic, Filters2, maps:merge(DbOpts, Opts3)}};
        {error, Error} ->
            {error, Error}
    end;

object_db_get_query(nkelastic, QueryType, _DbOpts) ->
    {error, {unknown_query_type, QueryType}};

object_db_get_query(Backend, _, _) ->
    {error, {unknown_query_backend, Backend}}.



%% @doc
object_create(#{?CHAT_CONVERSATION:=Conv} = Obj) ->
    Members = maps:get(initial_member_ids, Conv, []),
    case check_members(Members, []) of
        {ok, MemberIds} ->
            Conv2 = Conv#{
                members => [],
                invitations => [],
                initial_member_ids => MemberIds
            },
            Obj2 = ?ADD_TO_OBJ(?CHAT_CONVERSATION, Conv2, Obj),
            Obj3 = case Conv2 of
                #{obj_name_follows_members:=true} ->
                    ObjName = make_obj_name(make_members_hash(MemberIds)),
                    ?ADD_TO_OBJ(obj_name, ObjName, Obj2);
                _ ->
                    Obj2
            end,
            nkdomain_obj_make:create(Obj3);
        {error, Error} ->
            {error, Error}
    end.


%% @private
object_send_event(Event, State) ->
    nkchat_conversation_obj_events:event(Event, State).


%% @private
object_api_syntax(Cmd, Syntax) ->
    nkchat_conversation_obj_syntax:syntax(Cmd, Syntax).


%% @private
object_api_cmd(Cmd, Req) ->
    nkchat_conversation_obj_cmd:cmd(Cmd, Req).


% @private When the object is loaded, we make our cache
object_init(#obj_state{id=Id, obj=Obj}=State) ->
    #obj_id_ext{obj_id=ConvId} = Id,
    #{obj_name:=ObjName, ?CHAT_CONVERSATION := #{members:=MemberList, invitations:=InvitationList, type:=Type}=Conv} = Obj,
    Members = lists:map(
        fun(Data) ->
            #{
                member_id := MemberId,
                added_time := AddedTime,
                last_active_time := LastActiveTime,
                last_seen_message_time := LastSeenTime
            } = Data,
            #member{
                member_id = MemberId,
                added_time = AddedTime,
                roles = maps:get(member_roles, Data, []),
                last_active_time= LastActiveTime,
                last_seen_msg_time = LastSeenTime
            }
        end,
        MemberList),
    case read_messages(ConvId, State) of
        {ok, Total, Msgs} ->
            Msgs2 = lists:map(
                fun(#{<<"obj_id">>:=MsgId, <<"created_time">>:=Time}=Msg) ->
                    {Time, MsgId, Msg}
                end,
                Msgs),
            CreatedTime = maps:get(created_time, Obj, 0),
            LastMessageTime = case Msgs2 of
                [{MsgTime, _MsgId, _Msg}|_] ->
                    MsgTime;
                [] ->
                    CreatedTime
            end,
            Session = #session{
                name = maps:get(name, Obj, ObjName),
                type = Type,
                status = maps:get(status, Conv, <<>>),
                is_closed = maps:get(is_closed, Conv, false),
                members = Members,
                invitations = load_initial_invitations(InvitationList, []),
                total_messages = Total,
                messages = Msgs2,
                push_srv_id = maps:get(push_srv_id, Conv, <<>>),
                last_message_time = LastMessageTime,
                obj_name_follows_members = maps:get(obj_name_follows_members, Conv, false)
            },
            State2 = lists:foldl(
                fun(Data, StateN) ->
                    #{
                        member_id := MemberId2
                    } = Data,
                    do_event({conversation_loaded, MemberId2}, StateN)
                end,
                State#obj_state{session=Session},
                MemberList),
            case maps:take(initial_member_ids, Conv) of
                error ->
                    {ok, State2};
                {MemberIds, Conv2} when Members == [] ->
                    Obj2 = ?ADD_TO_OBJ(?CHAT_CONVERSATION, Conv2, Obj),
                    State3 = State2#obj_state{obj=Obj2, is_dirty=true},
                    % Store generated users
                    nkdomain_obj:async_op(self(), save),
                    nkdomain_obj_util:do_save_timer(load_initial_members(MemberIds, State3))
            end;
        {error, Error} ->
            {error, Error}
    end.


%% @private Prepare the object for saving
object_save(#obj_state{obj=Obj, session=Session}=State) ->
    #session{
        members = Members,
        invitations = Invitations,
        is_closed = IsClosed,
        last_message_time = LastMessageTime,
        status = Status
    } = Session,
    MemberList= lists:map(
        fun(Member) ->
            #member{
                member_id = MemberId,
                added_time = Time,
                roles = Roles,
                last_active_time = ActiveTime,
                last_seen_msg_time = MsgTime
            } = Member,
            #{
                member_id => MemberId,
                added_time => Time,
                member_roles => Roles,
                last_active_time => ActiveTime,
                last_seen_message_time => MsgTime
            }
        end,
        Members),
    InvitationList = lists:map(
        fun(Invitation) ->
            #invitation{
                token_id = TokenId,
                invited_by = InvitedBy,
                user_id = UserId,
                created_time = CreatedTime,
                expires_time = ExpiresTime
            } = Invitation,
            #{
                token_id => TokenId,
                invited_by => InvitedBy,
                user_id => UserId,
                created_time => CreatedTime,
                expires_time => ExpiresTime
            }
        end,
        Invitations),
    #{?CHAT_CONVERSATION:=Conv1} = Obj,
    Conv2 = Conv1#{
        members => MemberList,
        invitations => InvitationList,
        is_closed => IsClosed,
        last_message_time => LastMessageTime,
        status => Status
    },
    Obj2 = ?ADD_TO_OBJ(?CHAT_CONVERSATION, Conv2, Obj),
    {ok, State#obj_state{obj = Obj2}}.


% @private
object_sync_op({?MODULE, Op}, From, State) ->
    sync_op(Op, From, State);

object_sync_op(_Op, _From, _State) ->
    continue.


%% @private
object_async_op({?MODULE, Op}, State) ->
    async_op(Op, State);

object_async_op(_Op, _State) ->
    continue.


%% @private
object_link_down({usage, {?MODULE, session, MemberId, SessId, _Pid}}, State) ->
    case do_remove_session(MemberId, SessId, State) of
        {ok, State2} ->
            {ok, State2};
        {error, _} ->
            {ok, State}
    end;

object_link_down(_Link, State) ->
    {ok, State}.

%% @private Hook called before sending a event
object_event({invite_added, InviteData}, State) ->
    InviteData2 = maps:with([user_id, invited_by, created_time, expires_time], InviteData),
    {ok, do_event_all_sessions({invite_added, InviteData2}, State)};

object_event({invite_removed, UserId}, State) ->
    {ok, do_event_all_sessions({invite_removed, UserId}, State)};

object_event({is_closed_updated, IsClosed}, State) ->
    {ok, do_event_all_sessions({is_closed_updated, IsClosed}, State)};

object_event({last_seen_message, MemberIds, Time}, State) ->
    {ok, do_event_all_sessions({last_seen_message, MemberIds, Time}, State)};

object_event({member_added, MemberId, MemberData}, State) ->
    {ok, do_event_all_sessions({member_added, MemberId, MemberData}, State)};

object_event({member_removed, MemberId}, State) ->
    {ok, do_event_all_sessions({member_removed, MemberId}, State)};

object_event({member_muted, MemberId, Muted}, State) ->
    {ok, do_event_all_sessions({member_muted, MemberId, Muted}, State)};

object_event({member_typing, UserId}, State) ->
    {ok, do_event_all_sessions({member_typing, UserId}, State)};

object_event({message_created, Msg}, #obj_state{id=#obj_id_ext{obj_id=ConvId}, obj=Obj, session=Session, effective_srv_id=SrvId}=State) ->
    ?DEBUG("created message ~p", [Msg], State),
    spawn(fun() -> ?CALL_SRV(SrvId, nkchat_message_event, [ConvId, <<"created">>, Obj, Msg]) end),
    #session{total_messages=Total, messages=Msgs} = Session,
    #{obj_id:=MsgId, created_time:=Time} = Msg,
    Msgs2 = [{Time, MsgId, Msg}|Msgs],
    Msgs3 = case length(Msgs2) >= ?MSG_CACHE_SIZE of
        true -> lists:sublist(Msgs2, ?MSG_CACHE_SIZE);
        false -> Msgs2
    end,
    Session2 = Session#session{
        total_messages = Total+1,
        messages = Msgs3,
        last_message_time = Time
    },
    State2 = do_new_msg_event(Time, Msg, State#obj_state{session=Session2}),
    % is_dirty is already true
    %State3 = nkdomain_obj_util:do_save_timer(State2),
    % Save conversation immediately when a new message is created
    #obj_state{object_info=Info}=State2,
    State3 = nkdomain_obj_util:do_save_timer(State2#obj_state{object_info=Info#{save_time => 0}, save_timer = undefined}),
    % Restoring original save_time
    State4 = State3#obj_state{object_info=Info},
    {ok, State4};

object_event({message_updated, Msg}, #obj_state{id=#obj_id_ext{obj_id=ConvId}, obj=Obj, session=Session, effective_srv_id=SrvId}=State) ->
    spawn(fun() -> ?CALL_SRV(SrvId, nkchat_message_event, [ConvId, <<"updated">>, Obj, Msg]) end),
    #session{messages=Msgs} = Session,
    #{obj_id:=MsgId, created_time:=Time} = Msg,
    Session2 = case lists:keymember(MsgId, 2, Msgs) of
        true ->
            Msgs2 = lists:keystore(MsgId, 2, Msgs, {Time, MsgId, Msg}),
            Session#session{messages=Msgs2};
        false ->
            Session
    end,
    State2 = do_event_all_sessions({message_updated, Msg}, State#obj_state{session=Session2}),
    {ok, State2};

object_event({message_deleted, MsgId}, #obj_state{id=#obj_id_ext{obj_id=ConvId}, obj=Obj, session=Session, effective_srv_id=SrvId}=State) ->
    spawn(fun() -> ?CALL_SRV(SrvId, nkchat_message_event, [ConvId, <<"deleted">>, Obj, #{obj_id => MsgId}]) end),
    #session{total_messages=Total, messages=Msgs, last_message_time=LastMsgTime} = Session,
    CreatedTime = maps:get(created_time, Obj, 0),
    Session2 = case lists:keymember(MsgId, 2, Msgs) of
        true ->
            Msgs2 = lists:keydelete(MsgId, 2, Msgs),
            LastMsgTime2 = case Msgs2 of
                [{T, _, _}|_] ->
                    T;
                [] ->
                    CreatedTime
            end,
            Session#session{messages=Msgs2, last_message_time=LastMsgTime2};
        false ->
            Session
    end,
    Session3 = Session2#session{total_messages=Total-1},
    State2 = do_event_all_sessions({message_deleted, MsgId}, State#obj_state{session=Session3}),
    #session{last_message_time=NewLastMsgTime} = Session3,
    IsDirty = NewLastMsgTime =/= LastMsgTime,
    case IsDirty of
        true ->
            %State3 = nkdomain_obj_util:do_save_timer(State2#obj_state{is_dirty=true}),
            % Save conversation immediately when the last message was deleted
            #obj_state{object_info=Info}=State2,
            State3 = nkdomain_obj_util:do_save_timer(State2#obj_state{object_info=Info#{save_time => 0}, save_timer=undefined, is_dirty=true}),
            % Restoring original save_time
            State4 = State3#obj_state{object_info=Info},
            {ok, State4};
        false ->
            {ok, State2}
    end;

object_event({session_removed, MemberId, SessId}, State) ->
    {ok, do_event_all_sessions({session_removed, MemberId, SessId}, State)};

object_event({status_updated, Status}, State) ->
    {ok, do_event_all_sessions({status_updated, Status}, State)};

object_event({updated, _Update}, #obj_state{obj=#{?CHAT_CONVERSATION:=ChatConv}=Obj, session=Session} = State) ->
    #session{status=Status, is_closed=IsClosed} = Session,
    #{type:=Type, members:=Members} = ChatConv,
    Data = #{
        name => maps:get(name, Obj, <<>>),
        description => maps:get(description, Obj, <<>>),
        type => Type,
        is_closed => IsClosed,
        status => Status,
        members => Members,
        info => maps:get(info, ChatConv, [])
    },
    {ok, do_event_all_sessions({conversation_updated, Data}, State)};

object_event(_Event, State) ->
    {ok, State}.


%% @private
%%object_handle_info({'DOWN', _Ref, process, Pid, _Reason}, #obj_state{session=#session{sent_invitations=SentInvitations}}=State) ->
%%    case maps:is_key(Pid, SentInvitations) of
%%        true ->
%%            TokenId = maps:get(Pid, SentInvitations),
%%            State2 = case find_invitation(TokenId, State) of
%%                {true, #invitation{user_id = UserId}} ->
%%                    % This invitation was consumed by the user
%%                    {ok, NewState} = do_remove_invitation(TokenId, State),
%%                    %lager:warning("Sent invite_removed event: ~p~n", [UserId]),
%%                    do_event({invite_removed, UserId}, NewState);
%%                false ->
%%                    % This invitation was replaced with another
%%                    State
%%            end,
%%            SentInvitations2 = maps:remove(Pid, SentInvitations),
%%            #obj_state{session=Session2}=State2,
%%            State3 = nkdomain_obj_util:do_save_timer(State2#obj_state{session=Session2#session{sent_invitations=SentInvitations2}}),
%%            {noreply, State3};
%%        false ->
%%            lager:warning("[~p] Received DOWN for an unknown process ~p", [?MODULE, Pid]),
%%            continue
%%    end;

object_handle_info({'DOWN', _Ref, process, Pid, _Reason}, #obj_state{session=Session}=State) ->
    #session{invitations=Invitations} = Session,
    case lists:keytake(Pid, #invitation.pid, Invitations) of
        {value, #invitation{user_id=UserId}, Invitations2} ->
            Session2 = Session#session{invitations=Invitations2},
            State2 = State#obj_state{session=Session2, is_dirty=true},
            State3 = do_event({invite_removed, UserId}, State2),
            State4 = nkdomain_obj_util:do_save_timer(State3),
            {noreply, State4};
        false ->
            % lager:warning("[~p] Received DOWN for an unknown process ~p", [?MODULE, Pid]),
            continue
    end;

object_handle_info(_Info, _State) ->
    continue.



%% ===================================================================
%% Internal
%% ===================================================================

%% @private
sync_op({get_info}, _From, State) ->
    #obj_state{obj=#{?CHAT_CONVERSATION:=ChatConv}=Obj, session=Session} = State,
    #session{status=Status, is_closed=IsClosed} = Session,
    #{type:=Type, members:=Members} = ChatConv,
    Tags = maps:get(tags, Obj, []),
    Data = #{
        name => maps:get(name, Obj, <<>>),
        description => maps:get(description, Obj, <<>>),
        type => Type,
        is_closed => IsClosed,
        status => Status,
        members => Members,
        info => maps:get(info, ChatConv, []),
        tags => Tags
    },
    {reply, {ok, Data}, State};

%% @private
sync_op({get_status}, _From, State) ->
    #obj_state{domain_id=DomainId, id=#obj_id_ext{obj_id=ConvId}, session=Session} = State,
    #session{is_closed=IsClosed, status=Status} = Session,
    {reply, {ConvId, DomainId, Status, IsClosed}, State};

sync_op({set_status, Status}, _From, #obj_state{session=Session}=State) ->
    case Session of
        #session{status=Status} ->
            {reply, ok, State};
        _ ->
            Session2 = Session#session{status=Status},
            State2 = State#obj_state{session=Session2},
            State3 = do_event({status_updated, Status}, State2),
            {reply_and_save, ok, State3}
    end;

sync_op({set_closed, Closed}, _From, #obj_state{session=Session}=State) ->
    case Session of
        #session{is_closed=Closed} ->
            {reply, ok, State};
        _ ->
            Session2 = Session#session{is_closed=Closed},
            State2 = State#obj_state{session=Session2},
            State3 = do_event({is_closed_updated, Closed}, State2),
            {reply_and_save, ok, State3}
    end;

sync_op({add_info, Info}, _From, State) ->
    #obj_state{obj=#{?CHAT_CONVERSATION:=ChatConv}=Obj} = State,
    Infos1 = maps:get(info, ChatConv, []),
    Infos2 = [Info|Infos1],
    ChatConv2 = ChatConv#{info=>Infos2},
    Obj2 = ?ADD_TO_OBJ(?CHAT_CONVERSATION, ChatConv2, Obj),
    State2 = State#obj_state{obj=Obj2},
    State3 = do_event({added_info, Info}, State2),
    {reply_and_save, ok, State3};

sync_op({is_closed}, _From, State) ->
    #obj_state{domain_id=DomainId, id=#obj_id_ext{obj_id=ConvId}, session=Session} = State,
    #session{is_closed=IsClosed} = Session,
    {reply, {IsClosed, ConvId, DomainId}, State};

sync_op({set_status, #{is_closed:=Closed}}, _From, #obj_state{session=Session}=State) ->
    case Session of
        #session{is_closed=Closed} ->
            {reply, ok, State};
        _ ->
            Session2 = Session#session{is_closed=true},
            State2 = State#obj_state{session=Session2},
            {reply_and_save, ok, State2}
    end;

sync_op({set_status, _Status}, _From, State) ->
    {reply, {error, unknown_status}, State};

sync_op({add_member, MemberId, Opts}, _From, State) ->
    case do_add_member(MemberId, Opts, State) of
        {ok, State2} ->
            {reply_and_save, {ok, MemberId}, State2};
        {error, Error} ->
            {reply, {error, Error}, State}
    end;

sync_op({remove_member, MemberId}, _From, State) ->
    case find_member(MemberId, State) of
        {true, _} ->
            State2 = do_event({member_removed, MemberId}, State),              % Using original state
            case do_remove_member(MemberId, State2) of
                {ok, State3} ->
                    State4 = do_event({removed_from_conversation, MemberId}, State3),
                    {reply_and_save, ok, State4};
                {error, Error} ->
                    % Can fail if obj_name_follows_members
                    {reply, {error, Error}, State2}
            end;
        false ->
            {reply, {error, member_not_found}, State}
    end;

sync_op({remove_all_members}, _From, State) ->
    Members = get_members(State),
    {Reply, State2} = lists:foldl(fun(#member{member_id=MemberId}, {NReply, NState}) ->
        case NReply of
            {error, Err} ->
                {error, Err};
            ok ->
                NState2 = do_event({member_removed, MemberId}, NState),              % Using original state
                case do_remove_member(MemberId, NState2) of
                    {ok, NState3} ->
                        NState4 = do_event({removed_from_conversation, MemberId}, NState3),
                        {ok, NState4};
                    {error, Err} ->
                        % Can fail if obj_name_follows_members
                        {{error, Err}, NState2}
                end
        end
    end,
    {ok, State},
    Members
    ),
    case Reply of
        ok ->
            {reply_and_save, ok, State2};
        {error, Error} ->
            {reply, {error, Error}, State2}
    end;

sync_op({add_session, MemberId, SessId, Meta, Pid}, _From, State) ->
    case do_add_session(MemberId, SessId, Meta, Pid, State) of
        {ok, State2} ->
            State3 = do_event({session_added, MemberId, SessId}, State2),
            {reply, {ok, self()}, State3};
        {error, Error} ->
            {reply, {error, Error}, State}
    end;

sync_op({remove_session, UserId, SessId}, _From, State) ->
    case do_remove_session(UserId, SessId, State) of
        {ok, State2} ->
            {reply, ok, State2};
        {error, Error} ->
            {reply, {error, Error}, State}
    end;

sync_op({get_member_info, MemberId}, _From, State) ->
    case find_member(MemberId, State) of
        {true, Member} ->
            Info = do_get_member_info(Member, State),
            {reply, {ok, Info}, State};
        false ->
            {reply, {error, member_not_found}, State}
    end;

sync_op({get_member_cached_data, MemberId}, _From, State) ->
    case find_member(MemberId, State) of
        {true, #member{last_seen_msg_time=Last, unread_count=UnreadCount}=Member} ->
            {Member3, State3} = case UnreadCount of
                -1 ->
                    Count = find_unread(Last, State),
                    Member2 = Member#member{unread_count=Count},
                    State2 = set_member(MemberId, Member2, false, State),
                    {Member2, State2};
                _ ->
                    {Member, State}
            end,
            {reply, {ok, Member3}, State3};
        false ->
            {reply, {error, member_not_found}, State}
    end;

sync_op({get_last_messages}, _From, #obj_state{session=Session}=State) ->
    #session{total_messages=Total, messages=Messages} = Session,
    Messages2 = [M || {_Time, _Id, M} <- Messages],
    {reply, {ok, #{total=>Total, data=>Messages2}}, State};

sync_op({get_pretty_name}, _From, State) ->
    #obj_state{obj=#{?CHAT_CONVERSATION:=ChatConv}=Obj} = State,
    #{type:=Type, members:=Members} = ChatConv,
    Name = case nkchat_conversation:is_direct_conversation(Type) of
        true ->
            MemberIds = [MemberId || #{member_id := MemberId} <- Members],
            MemberNames = lists:map(
                fun(MId) ->
                    case nkdomain:get_name(MId) of
                        {ok, #{name := Name}} ->
                            Name;
                        _ ->
                            <<>>
                    end
                end,
                MemberIds),
            nklib_util:bjoin(MemberNames, <<", ">>);
        false ->
            maps:get(name, Obj, <<>>)
    end,
    Data = #{
        obj_id => maps:get(obj_id, Obj, <<>>),
        name => Name
    },
    {reply, {ok, Data}, State};

sync_op({mute, MemberId, Muted}, _From, State) ->
    case mute_member(MemberId, Muted, State) of
        {ok, _NewTags, NewState} ->
            NewState2 = do_event({member_muted, MemberId, Muted}, NewState),
            {reply, ok, NewState2};
        {error, Error, NewState} ->
            {reply, {error, Error}, NewState}
    end;

sync_op({is_muted, MemberId}, _From, State) ->
    case is_muted(MemberId, State) of
        {ok, IsMuted} ->
            {reply, {ok, IsMuted}, State};
        {error, Error, NewState} ->
            {reply, {error, Error}, NewState}
    end;

sync_op({make_invite_token, UserId, Member, TTL}, From, State) ->
    #obj_state{id=#obj_id_ext{obj_id=ConvId}, domain_id=DomainId} = State,
    TTL2 = case TTL of
        0 -> ?INVITE_TTL;
        _ -> TTL
    end,
    case nkdomain_db:find(Member) of
        #obj_id_ext{type=?DOMAIN_USER, obj_id=MemberId} ->
            Data = #{
                <<"op">> => <<"invite_member">>,
                <<"conversation_id">> => ConvId,
                <<"member_id">> => MemberId
            },
            % Since the parent is ours, it would block
            spawn_link(
                fun() ->
                    TokenOpts = #{
                        parent_id => ConvId,
                        created_by => UserId,
                        subtype => ?CHAT_CONVERSATION,
                        ttl => TTL2
                    },
                    Reply = case nkdomain_token_obj:create(DomainId, TokenOpts, Data) of
                        {ok, TokenId, _Pid, _Secs} ->
                            {ok, ConvId, TokenId, TTL2};
                        {error, Error} ->
                            {error, Error}
                    end,
                    gen_server:reply(From, Reply)
                end),
            {noreply, State};
        _ ->
            {reply, {error, member_invalid}, State}
    end;

sync_op({add_invite_op, User, Member, Base, Opts}, _From, State) ->
    #obj_state{id=#obj_id_ext{obj_id=ConvId}} = State,
    case nkdomain_db:find(User) of
        #obj_id_ext{obj_id=UserId} ->
            case find_member(UserId, State) of
                {true, _} ->
                    case nkdomain_db:find(Member) of
                        #obj_id_ext{obj_id=MemberId} ->
                            case find_member(MemberId, State) of
                                false ->
                                    ConvData1 = maps:get(?CHAT_CONVERSATION, Base, #{}),
                                    ConvData2 = ConvData1#{
                                        <<"add_member_op">> => #{
                                            <<"conversation_id">> => ConvId,
                                            <<"member_id">> => MemberId,
                                            <<"user_id">> => UserId,
                                            <<"opts">> => #{
                                                <<"silent">> => maps:get(silent, Opts, ?DEFAULT_SILENT),
                                                <<"read_previous">> => maps:get(read_previous, Opts, ?DEFAULT_READ_PREVIOUS)
                                            },
                                            <<"date">> => nkdomain_util:timestamp()
                                        }
                                    },
                                    Base2 = Base#{?CHAT_CONVERSATION => ConvData2},
                                    {reply, {ok, ConvId, MemberId, UserId, Base2}, State};
                                {true, _} ->
                                    {reply, {error, member_already_present}, State}
                            end;
                        _ ->
                            {reply, {error, member_invalid}, State}
                    end;
                false ->
                    {reply, {error, unauthorized}, State}
            end;
        _ ->
            {reply, {error, user_invalid}, State}
    end;

sync_op(_Op, _From, _State) ->
    continue.


%% @private
async_op({set_active, MemberId, SessId, Bool}, State) ->
    case find_member(MemberId, State) of
        {true, #member{sessions=ChatSessions, last_seen_msg_time=LastSeenMsg}=Member} ->
            case lists:keytake(SessId, #chat_session.session_id, ChatSessions) of
                {value, #chat_session{}=ChatSession, ChatSessions2} ->
                    ChatSession2 = ChatSession#chat_session{is_active=Bool},
                    ChatSessions3 = [ChatSession2|ChatSessions2],
                    Member2 = Member#member{sessions=ChatSessions3},
                    {Member3, IsDirty} = case Bool of
                        true ->
                            #obj_state{session=#session{messages=Msgs}} = State,
                            Time = case Msgs of
                                [{Time0, _, _}|_] -> Time0;
                                _ -> 0
                            end,
                            do_event_member_sessions(Member2, {counter_updated, 0}, State),
                            HaveUnread = case LastSeenMsg =:= Time of
                                false ->
                                    do_event({last_seen_message, [MemberId], Time}, State),
                                    true;
                                true ->
                                    false
                            end,
                            {Member2#member{
                                last_active_time = nkdomain_util:timestamp(),
                                last_seen_msg_time = Time,
                                unread_count = 0
                            }, HaveUnread};
                        false ->
                            {Member2, false}
                    end,
                    % Save state only when this user has unread messages
                    {noreply, set_member(MemberId, Member3, IsDirty, State)};
                false ->
                    ?LLOG(warning, "set_active for unknown session", [], State),
                    {noreply, State}
            end;
        false ->
            ?LLOG(warning, "set_active for unknown member", [], State),
            {noreply, State}
    end;

%% @private
%%async_op({add_invite, TokenId}, #obj_state{id=#obj_id_ext{obj_id=ConvId}}=State) ->
%%    case nkdomain_token_obj:get_token_data(TokenId) of
%%        {ok, #{domain_id:=_DomainId, data:=TokenData, created_time:=CreatedTime, expires_time:=ExpiresTime}} ->
%%            #{
%%                ?CHAT_CONVERSATION := #{
%%                    <<"add_member_op">> := #{
%%                        <<"conversation_id">> := ConvId,
%%                        <<"member_id">> := UserId,
%%                        <<"user_id">> := InvitedBy
%%                    }
%%                }
%%            }=TokenData,
%%            Invite = #invitation{
%%                token_id = TokenId,
%%                invited_by = InvitedBy,
%%                user_id = UserId,
%%                created_time = CreatedTime,
%%                expires_time = ExpiresTime
%%            },
%%            State2 = case find_invitation_by_user_id(UserId, State) of
%%                {true, #invitation{token_id=PreviousTokenId}} ->
%%                    case nkdomain_token_obj:consume_token(PreviousTokenId, rejected) of
%%                        {ok, _Data} ->
%%                            %lager:warning("Sent invite_removed event: User ~p~n", [UserId]),
%%                            do_event({invite_removed, UserId}, State);
%%                        {error, _Error} ->
%%                            State
%%                    end;
%%                false ->
%%                    State
%%            end,
%%            State3 = set_invitation_by_user_id(UserId, Invite, State2),
%%            {ok, State4} = monitor_token_process(TokenId, State3),
%%            InviteAdded = #{
%%                invited_by => InvitedBy,
%%                user_id => UserId,
%%                created_time => CreatedTime,
%%                expires_time => ExpiresTime
%%            },
%%            %lager:warning("Sent invite_added event: ~p~n", [InviteAdded]),
%%            {noreply, do_event({invite_added, InviteAdded}, nkdomain_obj_util:do_save_timer(State4))};
%%        {error, Error} ->
%%            {error, Error}
%%    end;

async_op({added_invitation, InvitedBy, UserId, TokenId}, State) ->
    case nkdomain_token_obj:get_token_data(TokenId) of
        {ok, #{pid:=Pid, created_time:=CreatedTime, expires_time:=ExpiresTime}} ->
            monitor(process, Pid),
            Invite = #invitation{
                token_id = TokenId,
                pid = Pid,
                invited_by = InvitedBy,
                user_id = UserId,
                created_time = CreatedTime,
                expires_time = ExpiresTime
            },
            State2 = case find_invitation_by_user_id(UserId, State) of
                {true, #invitation{token_id=PreviousTokenId}} ->
                    case nkdomain_token_obj:consume_token(PreviousTokenId, rejected) of
                        {ok, _Data} ->
                            %lager:warning("Sent invite_removed event: User ~p~n", [UserId]),
                            do_event({invite_removed, UserId}, State);
                        {error, _Error} ->
                            State
                    end;
                false ->
                    State
            end,
            State3 = set_invitation_by_user_id(UserId, Invite, State2),
            InviteAdded = #{
                invited_by => InvitedBy,
                user_id => UserId,
                created_time => CreatedTime,
                expires_time => ExpiresTime
            },
            %lager:warning("Sent invite_added event: ~p~n", [InviteAdded]),
            State4 = do_event({invite_added, InviteAdded}, State3),
            {noreply, nkdomain_obj_util:do_save_timer(State4)};
        {error, Error} ->
            ?LLOG(error, "added_invitation: getting token data for ~s: ~p", [TokenId, Error], State),
            {noreply, State}
    end;

async_op({typing, MemberId}, State) ->
    case find_member(MemberId, State) of
        {true, #member{member_id=UserId, sessions=Sessions}} ->
            case lists:keymember(true, #chat_session.is_active, Sessions) of
                true ->
                    %?LLOG(info, "Sending member_typing event from ~s to all users", [MemberId], State),
                    State2 = do_event({member_typing, UserId}, State),
                    {noreply, State2};
                false ->
                    ?LLOG(error, "typing: conversation not active for member ~s", [MemberId], State),
                    {noreply, State}
            end;
        false ->
            ?LLOG(error, "typing: member ~s not found", [MemberId], State),
            {noreply, State}
    end;

async_op(_Op, _State) ->
    continue.


%% @private
load_initial_members(MemberIds, #obj_state{session=#session{obj_name_follows_members=true}=Session}=State) ->
    State2 = State#obj_state{session=Session#session{obj_name_follows_members=false}},
    % We don't want to update obj_name, it is already calculated for the initial user list
    case do_add_members(MemberIds, State2) of
        {ok, #obj_state{session=Session3}=State3} ->
            % From now on we keep updating
            {ok, State3#obj_state{session=Session3#session{obj_name_follows_members=true}}};
        {error, Error} ->
            {error, Error}
    end;

load_initial_members(MemberIds, State) ->
    do_add_members(MemberIds, State).


%%%% @private
%%load_initial_invitations(Invitations, State) ->
%%    load_initial_invitations(Invitations, [], State).
%%
%%load_initial_invitations([], ReversedInvs, State) ->
%%    Invs = lists:reverse(ReversedInvs),
%%    #obj_state{obj=#{?CHAT_CONVERSATION:=ChatConv}=Obj, session=Session}=State,
%%    ChatConv2 = ChatConv#{invitations=>Invs},
%%    RevInvs = lists:map(
%%        fun(Data) ->
%%            #{
%%                token_id := TokenId,
%%                invited_by := InvitedBy,
%%                user_id := UserId,
%%                created_time := CreatedTime,
%%                expires_time := ExpiresTime
%%            } = Data,
%%            #invitation{
%%                token_id = TokenId,
%%                invited_by = InvitedBy,
%%                user_id = UserId,
%%                created_time = CreatedTime,
%%                expires_time = ExpiresTime
%%            }
%%        end,
%%        Invs),
%%    State#obj_state{obj=Obj#{?CHAT_CONVERSATION=>ChatConv2}, session=Session#session{invitations=RevInvs}};
%%
%%load_initial_invitations([#{token_id:=TokenId}=Invitation|Invitations], CheckedInvs, State) ->
%%    case monitor_token_process(TokenId, State) of
%%        {ok, State2} ->
%%            load_initial_invitations(Invitations, [Invitation|CheckedInvs], State2);
%%        {error, _Error} ->
%%            load_initial_invitations(Invitations, CheckedInvs, State#obj_state{is_dirty=true})
%%    end.



%% @private
load_initial_invitations([], Acc) ->
    Acc;

load_initial_invitations([Invitation1|Rest], Acc) ->
    #{
        token_id := TokenId,
        invited_by := InvitedBy,
        user_id := UserId,
        created_time := CreatedTime,
        expires_time := ExpiresTime
    } = Invitation1,
    case nkdomain:load(TokenId) of
        {ok, ?DOMAIN_TOKEN, _ObjId, _Path, Pid} ->
            monitor(process, Pid),
            Invitation2 = #invitation{
                token_id = TokenId,
                pid = Pid,
                invited_by = InvitedBy,
                user_id = UserId,
                created_time = CreatedTime,
                expires_time = ExpiresTime
            },
            load_initial_invitations(Rest, [Invitation2|Acc]);
        {error, _} ->
            load_initial_invitations(Rest, Acc)
    end.



%%%% @private
%%monitor_token_process(TokenId, #obj_state{session=#session{sent_invitations=SentInvitations}=Session}=State) ->
%%    case nkdomain:load(TokenId) of
%%        {ok, ?DOMAIN_TOKEN, ObjId, _Path, Pid} ->
%%            monitor(process, Pid),
%%            SentInvitations2 = SentInvitations#{Pid => ObjId},
%%            {ok, State#obj_state{session=Session#session{sent_invitations=SentInvitations2}}};
%%        {error, Error} ->
%%            {error, Error}
%%    end.




%% @private
do_add_members([], State) ->
    {ok, State};

do_add_members([MemberId|Rest], State) ->
    case do_add_member(MemberId, State) of
        {ok, State2} ->
            do_add_members(Rest, State2);
        {error, Error} ->
            {error, Error}
    end.


%% @private
do_add_member(MemberId, State) ->
    do_add_member(MemberId, #{}, State).

%% @private
do_add_member(MemberId, Opts, State) ->
    case find_member(MemberId, State) of
        false ->
            #obj_state{session=#session{messages=Msgs}} = State,
            Time = case Msgs of
                [{Time0, _, _}|_] -> Time0;
                _ -> 0
            end,
            ReadPrevious = maps:get(read_previous, Opts, false),
            Now = nkdomain_util:timestamp(),
            {Member, MemberData} = case ReadPrevious of
                true ->
                    Member1 = #member{
                        member_id = MemberId,
                        added_time = Now,
                        last_seen_msg_time = Time,
                        last_active_time = Now
                    },
                    MemberData1 = #{
                        member_id => MemberId,
                        added_time => Now,
                        member_roles => [],
                        last_active_time => Now,
                        last_seen_message_time => Time,
                        is_muted => false
                    },
                    {Member1, MemberData1};
                _ ->
                    Member1 = #member{
                        member_id = MemberId,
                        added_time = Now
                    },
                    MemberData1 = #{
                        member_id => MemberId,
                        added_time => Now,
                        member_roles => [],
                        last_active_time => 0,
                        last_seen_message_time => 0,
                        is_muted => false
                    },
                    {Member1, MemberData1}
            end,
            State2 = set_member(MemberId, Member, State),
            case set_obj_name_members(State2) of
                {ok, State3} ->
                    State4 = do_event({member_added, MemberId, MemberData}, State3),
                    State5 = do_event({added_to_conversation, MemberId}, State4),
                    {ok, State5};
                {error, Error} ->
                    {error, Error}
            end;
        {true, _} ->
            {error, member_already_present}
    end.


%% @private
do_remove_member(MemberId, State) ->
    case find_member(MemberId, State) of
        {true, #member{sessions=Sessions}} ->
            State2 = rm_member(MemberId, State),
            case set_obj_name_members(State2) of
                {ok, State3} ->
                    State4 = do_remove_sessions(MemberId, Sessions, State3),
                    {ok, State4};
                {error, Error} ->
                    {error, Error}               % Old state
            end;
        false ->
            {error, member_not_found}
    end.


%%%% @private
%%do_remove_invitation(InvitationId, State) ->
%%    case find_invitation(InvitationId, State) of
%%        {true, _Invitation} ->
%%            State2 = rm_invitation(InvitationId, State),
%%            {ok, State2};
%%        false ->
%%            {error, object_not_found}
%%    end.


%% @private
do_add_session(MemberId, SessId, Meta, Pid, State) ->
    case find_member(MemberId, State) of
        {true, #member{sessions=Sessions}=Member} ->
            Session = #chat_session{session_id=SessId, meta=Meta, pid=Pid, is_active=false},
            Sessions2 = [Session|Sessions],
            Member2 = Member#member{sessions=Sessions2},
            Member3 = case Member2 of
                #member{last_seen_msg_time=Last, unread_count=-1} ->
                    Count = find_unread(Last, State),
                    % This counter_updated event is not needed at this time
                    %do_event_member_sessions(Member2, {counter_updated, Count}, State),
                    Member2#member{unread_count=Count};
                #member{unread_count=_Count} ->
                    % This counter_updated event is not needed at this time
                    %do_event_member_sessions(Member2, {counter_updated, _Count}, State),
                    Member2
            end,
            State3 = set_member(MemberId, Member3, false, State),
            State4 = nkdomain_obj:links_add(usage, {?MODULE, session, MemberId, SessId, Pid}, State3),
            {ok, State4};
        false ->
            {error, member_not_found}
    end.


%% @private
do_remove_session(MemberId, SessId, State) ->
    case find_member(MemberId, State) of
        {true, #member{sessions=Sessions1}=Member} ->
            case lists:keytake(SessId, #chat_session.session_id, Sessions1) of
                {value, #chat_session{pid=Pid}, Sessions2} ->
                    State2 = nkdomain_obj:links_remove(usage, {?MODULE, session, MemberId, SessId, Pid}, State),
                    State3 = do_event({session_removed, MemberId, SessId}, State2),
                    Member2 = Member#member{sessions=Sessions2},
                    {ok, set_member(MemberId, Member2, false, State3)};
                false ->
                    {error, session_not_found}
            end;
        false ->
            {error, session_not_found}
    end.


%% @private
do_remove_sessions(_MemberId, [], #obj_state{} = State) ->
    State;

do_remove_sessions(MemberId, [#chat_session{session_id=SessId}|Rest], State) ->
    case do_remove_session(MemberId, SessId, State) of
        {ok, State2} ->
            do_remove_sessions(MemberId, Rest, State2);
        {error, Error} ->
            lager:error("NKLOG do_remove_sessions ~p", [Error]),
            do_remove_sessions(MemberId, Rest, State)
    end.


%% @private
find_member(MemberId, State) ->
    Members = get_members(State),
    case lists:keyfind(MemberId, #member.member_id, Members) of
        #member{}=Member ->
            {true, Member};
        false ->
            false
    end.


%% @private
get_members(#obj_state{session=Session}) ->
    #session{members=Members} = Session,
    Members.


%% @private
set_members(Members, State) ->
    set_members(Members, true, State).

%% @private
set_members(Members, IsDirty, #obj_state{id=_Id, session=Session, is_dirty=OldIsDirty}=State) ->
    Session2 = Session#session{members=Members},
    State#obj_state{session=Session2, is_dirty=(IsDirty orelse OldIsDirty)}.

%% @private
set_member(MemberId, Member, State) ->
    set_member(MemberId, Member, true, State).

%% @private
set_member(MemberId, Member, IsDirty, State) ->
    Members1 = get_members(State),
    Members2 = lists:keystore(MemberId, #member.member_id, Members1, Member),
    set_members(Members2, IsDirty, State).


%% @private
rm_member(MemberId, State) ->
    {_, _, State2} = mute_member(MemberId, false, State),
    Members1 = get_members(State2),
    Members2 = lists:keydelete(MemberId, #member.member_id, Members1),
    set_members(Members2, State2).


%%%% @private
%%find_invitation(InvitationId, State) ->
%%    Invitations = get_invitations(State),
%%    case lists:keyfind(InvitationId, #invitation.token_id, Invitations) of
%%        #invitation{}=Invitation ->
%%            {true, Invitation};
%%        false ->
%%            false
%%    end.


%% @private
find_invitation_by_user_id(UserId, State) ->
    Invitations = get_invitations(State),
    case lists:keyfind(UserId, #invitation.user_id, Invitations) of
        #invitation{}=Invitation ->
            {true, Invitation};
        false ->
            false
    end.


%% @private
get_invitations(#obj_state{session=Session}) ->
    #session{invitations=Invitations} = Session,
    Invitations.


%% @private
set_invitations(Invitations, #obj_state{session=Session}=State) ->
    Session2 = Session#session{invitations=Invitations},
    State#obj_state{session=Session2, is_dirty=true}.


%%%% @private
%%set_invitation(InvitationId, Invitation, State) ->
%%    Invitations1 = get_invitations(State),
%%    Invitations2 = lists:keystore(InvitationId, #invitation.token_id, Invitations1, Invitation),
%%    set_invitations(Invitations2, State).


%% @private
set_invitation_by_user_id(UserId, Invitation, State) ->
    Invitations1 = get_invitations(State),
    Invitations2 = lists:keystore(UserId, #invitation.user_id, Invitations1, Invitation),
    set_invitations(Invitations2, State).


%%%% @private
%%rm_invitation(InvitationId, State) ->
%%    Invitations1 = get_invitations(State),
%%    Invitations2 = lists:keydelete(InvitationId, #invitation.token_id, Invitations1),
%%    set_invitations(Invitations2, State).



%% @private
do_event(Event, State) ->
    nkdomain_obj_util:event(Event, State).


%% @private
do_event_all_sessions(Event, State) ->
    Members = get_members(State),
    do_event_all_sessions(Members, Event, State).


%% @private
do_event_all_sessions([], _Event, State) ->
    State;

do_event_all_sessions([Member|Rest], Event, State) ->
    do_event_member_sessions(Member, Event, State),
    do_event_all_sessions(Rest, Event, State).


%% @private
do_event_member_sessions(#member{sessions=Sessions}, Event, State) ->
    do_event_sessions(Sessions, Event, State).


%% @private
do_event_sessions(Sessions, Event, #obj_state{id=Id}) when is_list(Sessions) ->
    #obj_id_ext{obj_id=ConvId} = Id,
    lists:foreach(
        fun(#chat_session{meta=Meta, pid=Pid}) ->
            nkchat_session_obj:conversation_event(Pid, ConvId, Meta, Event)
        end,
        Sessions).


%% @private
do_new_msg_event(Time, Msg, #obj_state{obj=#{?CHAT_CONVERSATION:=_ChatConv}} = State) ->
    Members = get_members(State),
    MembersIds = [M#member.member_id || M <- Members],
    MembersData = [{Id, get_member_data(Id, all)} || Id <- MembersIds],
    Opts = #{
        members_map => maps:from_list(MembersData)
    },
    State2 = do_new_msg_event(Members, Time, Msg, [], Opts, State),
    last_seen_msg_event(Time, State2).


%% @private
do_new_msg_event([], _Time, _Msg, Acc, _Opts, State) ->
    set_members(Acc, State);

do_new_msg_event([Member|Rest], Time, Msg, Acc, #{members_map := MembersMap} = Opts, #obj_state{obj=#{?CHAT_CONVERSATION:=ChatConv}=Obj} = State) ->
    #member{
        member_id = MemberId,
        last_seen_msg_time = Last,
        unread_count = Count,
        sessions = Sessions
    } = Member,
    IsActive = lists:keymember(true, #chat_session.is_active, Sessions),
    #{created_by:=CreatedBy} = Msg,
    IsFromThatUser = MemberId =:= CreatedBy,
    SendPush = case Sessions of
        [] ->
            true;
        _ when IsFromThatUser ->
            false;
        _ ->
            case nkdomain_user:get_presence(MemberId, ?CHAT_SESSION) of
                {ok, #{status := P}} when P =:= <<"inactive">>; P =:= <<"offline">> ->
                    true;
                _ ->
                    false
            end
    end,
    PushCount = case SendPush of
        true ->
            PCount = case Count of
                -1 ->
                    find_unread(Last, State) + 1;
                _ ->
                    Count + 1
            end,
            #{type:=ConvType} = ChatConv,
            #{?CHAT_MESSAGE:=#{text:=Txt, type:=MsgType}=MsgData} = Msg,
            MsgBody = maps:get(body, MsgData, #{}),
            #obj_state{session=#session{members=_Members, name=Name}, id=#obj_id_ext{obj_id=ConvId}} = State,
            IsMuted = case is_muted(MemberId, State) of
                {ok, Muted} ->
                    Muted;
                {error, _Error} ->
                    false
            end,
            PCount2 = case nkchat_conversation:get_unread_counter(<<"/">>, MemberId, #{}) of
                {ok, UC} ->
                    UC + PCount;
                _ ->
                    PCount
            end,
            ParentId = maps:get(parent_id, Obj, <<>>),
            Push = #{
                type => ?CHAT_CONVERSATION,
                class => new_msg,
                parent_id => ParentId,
                conversation_id => ConvId,
                conversation_name => Name,
                conversation_type => ConvType,
                created_by => CreatedBy,
                member_id => MemberId,
                members_map => MembersMap,
                message_body => MsgBody,
                message_text => Txt,
                message_type => MsgType,
                is_muted => IsMuted,
                unread_counter => PCount2
            },
            send_push(MemberId, Push, State),
            PCount;
        false ->
            Count
    end,
    Acc2 = case Sessions of
        [] ->
            Count2 = PushCount,
            Member2 = Member#member{
                unread_count = Count2
            },
            [Member2|Acc];
        _ when IsActive ->
            Member2 = Member#member{
                last_seen_msg_time = Time,
                unread_count = 0
            },
            do_event_member_sessions(Member2, {message_created, Msg}, State),
            case Count of
                0 ->
                    ok;
                _ ->
                    do_event_member_sessions(Member2, {counter_updated, 0}, State)
            end,
            [Member2|Acc];
        _ when IsFromThatUser ->
            Member2 = Member#member{
                unread_count = Count
            },
            do_event_member_sessions(Member2, {message_created, Msg}, State),
            [Member2|Acc];
        _ ->
            Count2 = Count + 1,
            Member2 = Member#member{
                unread_count = Count2
            },
            do_event_member_sessions(Member2, {message_created, Msg}, State),
            do_event_member_sessions(Member2, {counter_updated, Count2}, State),
            [Member2|Acc]
    end,
    do_new_msg_event(Rest, Time, Msg, Acc2, Opts, State).


%% @private
last_seen_msg_event(Time, State) ->
    Members = get_members(State),
    ActiveMembers = key_filter(Time, #member.last_seen_msg_time, Members),
    ActiveMembersIds = [M#member.member_id || M <- ActiveMembers],
    case ActiveMembersIds of
        [] ->
            State;
        _ ->
            do_event({last_seen_message, ActiveMembersIds, Time}, State)
    end.


%% @private
key_filter(Key, N, TupleList) ->
    lists:filter(fun(T) -> element(N, T) =:= Key end, TupleList).


%% @private
read_messages(ConvId, _State) ->
    case nkdomain_db:search(?CHAT_CONVERSATION, {query_conversation_messages, ConvId, #{size=>?MSG_CACHE_SIZE}}) of
        {ok, 0, [], _Meta} ->
            {ok, 0, []};
        {ok, Total, Objs, _Meta} ->
            {ok, Total, Objs};
        {error, Error} ->
            {error, Error}
    end.


%% @private
find_unread(Time, #obj_state{id=Id}=State) ->
    #obj_id_ext{obj_id=ConvId} = Id,
    case nkdomain_db:search(?CHAT_CONVERSATION, {query_conversation_messages, ConvId, #{size=>0, start_date=>Time}}) of
        {ok, Num, [], _Meta} ->
            Num;
        {error, Error} ->
            ?LLOG(error, "error reading unread count: ~p", [Error], State),
            0
    end.


%% @private
set_obj_name_members(#obj_state{obj=#{?CHAT_CONVERSATION:=Conv}=Obj, session=Session}=State) ->
    #obj_state{session=#session{members=Members}} = State,
    MemberIds = [Id || #member{member_id=Id} <- Members],
    Hash = make_members_hash(MemberIds),
    Conv2 = Conv#{members_hash => Hash},
    Obj2 = ?ADD_TO_OBJ(?CHAT_CONVERSATION, Conv2, Obj),
    State2 = State#obj_state{obj=Obj2, is_dirty=true},
    case Session of
        #session{obj_name_follows_members=true} ->
            ObjName = make_obj_name(Hash),
            nkdomain_obj:do_update_name(ObjName, State2);
        _ ->
            {ok, State2}
    end.


%% @private
make_members_hash(MemberIds) ->
    MemberIds2 = lists:usort(MemberIds),
    nkdomain_util:name(base64:encode(crypto:hash(sha, erlang:term_to_binary(MemberIds2)))).


%% @private
make_obj_name(Hash) ->
    <<"mh-", Hash/binary>>.


%% @private
do_get_member_info(Member, State) ->
    #obj_state{obj=Obj, session=Session} = State,
    #session{total_messages=Total, messages=Msgs, invitations=SessionInvs, members = CachedMembers} = Session,
    LastMessage = case Msgs of
        [{_, _, Msg}|_] -> Msg;
        [] -> #{}
    end,
    #member{unread_count=Counter} = Member,
    Invitations = lists:map(
        fun(Inv) ->
            #invitation{
                user_id = UserId,
                invited_by = InvitedBy,
                created_time = CreatedTime,
                expires_time = ExpiresTime
            }=Inv,
            #{
                user_id => UserId,
                invited_by => InvitedBy,
                created_time => CreatedTime,
                expires_time => ExpiresTime
            }
        end,
        SessionInvs
    ),
    #{
        path:=Path,
        parent_id:=ParentId,
        created_by:=CreatedBy,
        created_time:=CreatedTime,
        ?CHAT_CONVERSATION:=#{
            type:=Type,
            %members:=Members,
            is_closed:=IsClosed,
            info:=Info,
            status:=Status
        }
    } = Obj,
    CachedMembers2 = lists:map(
        fun(M) ->
            #member{
                member_id = MemberId,
                added_time = Time,
                roles = Roles,
                last_active_time = ActiveTime,
                last_seen_msg_time = MsgTime
            } = M,
            #{
                member_id => MemberId,
                added_time => Time,
                member_roles => Roles,
                last_active_time => ActiveTime,
                last_seen_message_time => MsgTime
            }
        end,
        CachedMembers),
    Members2 = parse_muted_members(CachedMembers2, State),
    #{
        parent_id => ParentId,
        name => maps:get(name, Obj, <<>>),
        description => maps:get(description, Obj, <<>>),
        type => Type,
        created_by => CreatedBy,
        created_time => CreatedTime,
        info => Info,
        status => Status,
        invitations => Invitations,
        is_closed => IsClosed,
        members => Members2,
        path => Path,
        total_messages => Total,
        unread_counter => Counter,
        last_message => LastMessage
    }.


%% @private
send_push(MemberId, Push, #obj_state{session=#session{push_srv_id=SrvId}}) ->
    nkdomain_user:send_push(MemberId, SrvId, Push).


%% @private
check_members([], Acc) ->
    {ok, Acc};

check_members([Member|Rest], Acc) ->
    case nkdomain_db:find(Member) of
        #obj_id_ext{type=?DOMAIN_USER, obj_id=MemberId} ->
            check_members(Rest, [MemberId|Acc]);
        _ ->
            {error, member_not_found}
    end.


%% @private
get_member_data(UserId, Field) ->
    case nkdomain_user:get_name(UserId) of
        {ok, #{obj_id := UserId}=User} ->
            case Field of
                all ->
                    User;
                _ ->
                    maps:get(Field, User, <<>>)
            end;
        {error, Error2} ->
            lager:warning("nkchat_conversation: could not find user obj ~p: ~p", [UserId, Error2]),
            case Field of
                all ->
                    #{};
                _ ->
                    <<>>
            end
    end.


%% @private
mute_member(MemberId, Muted, State) ->
    case find_member(MemberId, State) of
        {true, _} ->
            MutedTag = nkchat_conversation:get_muted_tag(MemberId),
            case Muted of
                true ->
                    add_tag(MutedTag, State);
                false ->
                    remove_tag(MutedTag, State)
            end;
        false ->
            {error, member_not_found, State}
    end.


%% @private
parse_muted_members(Members, State) ->
    parse_muted_members(Members, [], State).

parse_muted_members([], Acc, _State) ->
    lists:reverse(Acc);

parse_muted_members([#{member_id := MemberId}=M|Members], Acc, State) ->
    case is_muted(MemberId, State) of
        {ok, Muted} ->
            parse_muted_members(Members, [M#{is_muted=>Muted}|Acc], State);
        {error, _Error} ->
            parse_muted_members(Members, [M#{is_muted=>false}|Acc], State)
    end.


%% @private
is_muted(MemberId, State) ->
    case find_member(MemberId, State) of
        {true, _} ->
            MutedTag = nkchat_conversation:get_muted_tag(MemberId),
            has_tag(MutedTag, State);
        false ->
            {error, member_not_found}
    end.


%% @private
add_tag(Tag, #obj_state{obj=Obj}=State) ->
    Tags = maps:get(tags, Obj, []),
    case lists:member(Tag, Tags) of
        true ->
            {ok, Tags, State};
        false ->
            Tags2 = [Tag|Tags],
            State2 = State#obj_state{obj=Obj#{tags=>Tags2}, is_dirty=true},
            State3 = nkdomain_obj_util:do_save_timer(State2),
            {ok, Tags2, State3}
    end.


%% @private
remove_tag(Tag, #obj_state{obj=Obj}=State) ->
    Tags = maps:get(tags, Obj, []),
    case lists:member(Tag, Tags) of
        true ->
            Tags2 = lists:delete(Tag, Tags),
            State2 = State#obj_state{obj=Obj#{tags=>Tags2}, is_dirty=true},
            State3 = nkdomain_obj_util:do_save_timer(State2),
            {ok, Tags2, State3};
        false ->
            {ok, Tags, State}
    end.


%% @private
has_tag(Tag, #obj_state{obj=Obj}) ->
    Tags = maps:get(tags, Obj, []),
    {ok, lists:member(Tag, Tags)}.
