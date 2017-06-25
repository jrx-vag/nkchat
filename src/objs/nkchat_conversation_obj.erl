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

-export([create/3]).
-export([add_member/3, remove_member/3, add_session/5, set_session_active/5, remove_session/4]).
-export([get_messages/3, get_member_conversations/4]).
-export([message_event/3]).
-export([object_info/0, object_es_mapping/0, object_parse/3, object_create/2,
         object_api_syntax/2, object_api_cmd/2, object_send_event/2,
         object_init/1, object_save/1, object_sync_op/3, object_async_op/2,
         object_event/2, object_link_down/2]).
-export([object_admin_info/0]).
-export_type([event/0, class/0]).

-include("nkchat.hrl").
-include_lib("nkdomain/include/nkdomain.hrl").
-include_lib("nkdomain/include/nkdomain_debug.hrl").
-include_lib("nkservice/include/nkservice.hrl").


%% ===================================================================
%% Types
%% ===================================================================



-type event() ::
    {message_created, nkdomain:obj()} |
    {message_updated, nkdomain:obj()} |
    {message_deleted, nkdomain:obj_id()} |
    {member_added, nkdomain:obj_id()} |
    {added_to_conversation, nkdomain:obj_id()} |        % Same but obj_id is for the member
    {member_removed, nkdomain:obj_id()} |
    {removed_from_conversation, nkdomain:obj_id()} |    % Same but obj_id is for the member
    {session_added, Member::nkdomain:obj_id(), SessId::nkdomain:obj_id()} |
    {session_removed, Member::nkdomain:obj_id(), SessId::nkdomain:obj_id()}.


-type class() :: channel | private | one2one.


%% ===================================================================
%% Public
%% ===================================================================


%% @doc
create(SrvId, Domain, Name) ->
    Obj = #{
        type => ?CHAT_CONVERSATION,
        domain_id => Domain,
        created_by => <<"admin">>,
        obj_name => Name
    },
    nkdomain_obj_make:create(SrvId, Obj).



%% @doc Members will be changed for roles

-spec add_member(nkservice:id(), nkdomain:id(), nkdomain:id()) ->
    {ok, nkdomain:obj_id()} | {error, term()}.

add_member(SrvId, Id, Member) ->
    case nkdomain_lib:find(SrvId, Member) of
        #obj_id_ext{obj_id=MemberId} ->
            nkdomain_obj:sync_op(SrvId, Id, {?MODULE, add_member, MemberId});
        {error, object_not_found} ->
            {error, user_not_found};
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



%% @private Called from nkchat_session_obj
%% Sessions receive notifications for every message, calling
-spec add_session(nkservice:id(), nkdomain:obj_id(), nkdomain:obj_id(), nkdomain:obj_id(), map()) ->
    {ok, pid()} | {error, term()}.

add_session(SrvId, ConvId, MemberId, SessId, Meta) ->
    nkdomain_obj:sync_op(SrvId, ConvId, {?MODULE, add_session, MemberId, SessId, Meta, self()}).


%% @private
set_session_active(SrvId, ConvId, MemberId, SessId, Bool) when is_boolean(Bool)->
    nkdomain_obj:async_op(SrvId, ConvId, {?MODULE, set_active, MemberId, SessId, Bool}).


%% @private Called from nkchat_session_obj
%% Sessions receive notifications for every message, calling
remove_session(SrvId, ConvId, MemberId, SessId) ->
    nkdomain_obj:sync_op(SrvId, ConvId, {?MODULE, remove_session, MemberId, SessId}).


%% @doc
get_member_conversations(SrvId, Domain, Member, Full) ->
    case nkdomain_lib:find(SrvId, Domain) of
        #obj_id_ext{srv_id=SrvId, path=DomainPath} ->
            case nkdomain_lib:find(SrvId, Member) of
                #obj_id_ext{obj_id=MemberId} ->
                    Filters = #{
                        type => ?CHAT_CONVERSATION,
                        path => <<"childs_of:", DomainPath/binary>>,
                        << ?CHAT_CONVERSATION/binary, ".members.member_id">> => MemberId
                    },
                    Fields = case Full of
                        true ->
                            [
                                created_by, created_time, description, name, path,
                                <<?CHAT_CONVERSATION/binary, ".members.member_id">>, <<?CHAT_CONVERSATION/binary, ".class">>
                            ];
                        false ->
                            []
                    end,
                    Search2 = #{
                        sort => [#{created_time => #{order => desc}}],
                        fields => Fields,
                        filters => Filters,
                        size => 9999
                    },
                    lager:error("NKLOG SS ~p", [Search2]),
                    case nkdomain:search(SrvId, Search2) of
                        {ok, N, List, _Meta} ->
                            {ok, #{total=>N, data=>List}};
                        {error, Error} ->
                            {error, Error}
                    end;
                {error, object_not_found} ->
                    {error, member_unknown};
                {error, Error} ->
                    {error, Error}
            end;
        {error, object_not_found} ->
            {error, domain_unknown};
        {error, Error} ->
            {error, Error}
    end.


%% @doc
get_messages(SrvId, Id, Spec) ->
    case nkdomain_lib:load(SrvId, Id) of
        #obj_id_ext{obj_id=ConvId} ->
            Search1 = maps:with([from, size], Spec),
            Filters1 = #{
                type => ?CHAT_MESSAGE,
                domain_id => ConvId
            },
            Filters2 = case Spec of
                #{start_date:=Date} ->
                    Filters1#{created_time => {Date, none}};
                _ ->
                    Filters1
            end,
            Search2 = Search1#{
                sort => [#{created_time => #{order => desc}}],
                fields => [created_time, ?CHAT_MESSAGE, created_by],
                filters => Filters2
            },
            case nkdomain:search(SrvId, Search2) of
                {ok, N, List, _Meta} ->
                    List2 = lists:map(
                        fun(#{<<"created_by">>:=MemberId}=D) -> append_user_data(SrvId, MemberId, D) end,
                        List),
                    {ok, #{total=>N, data=>List2}};
                {error, Error} ->
                    {error, Error}
            end;
        {error, object_not_found} ->
            {error, conversation_not_found};
        {error, Error} ->
            {error, Error}
    end.


%% @private Called from nkchat_message_obj
%% The message object generates an event that inserts into the conversation object
%% and it sends it as if it were an own message
%% Is is captured at object_event/2 and sent to the sessions in send_to_sessions/3
message_event(SrvId, ConvId, Event) ->
    Event2 = case Event of
        {created, Msg} ->
            {message_created, Msg};
        {deleted, MsgId} ->
            {message_deleted, MsgId};
        {updated, Msg} ->
            {message_updated, Msg};
        _ ->
            ignore
    end,
    case Event2 of
        ignore ->
            ok;
        _ ->

            nkdomain_obj:async_op(SrvId, ConvId, {send_event, Event2})
    end.


%%%% @private
%%session_event_filter(Event) ->
%%    case Event of
%%        {message_created, Msg} -> {new, Msg};
%%        {message_updated, Msg} -> {updated, Msg};
%%        {message_deleted, MsgId} -> {deleted, MsgId};
%%        {member_added, _} -> async;
%%        {member_removed, _} -> async;
%%        _ ->
%%            %% lager:debug("EV Other: ~p", [Event]),
%%            false
%%    end.




%% =================================================================
%% nkdomain_obj behaviour
%% ===================================================================

-record(chat_session, {
    session_id :: nkdomain:obj_id(),
    meta :: map(),
    pid ::pid(),
    is_active :: boolean()
}).

-record(member, {
    member_id :: nkdomain:obj_id(),
    added_time :: nklib_util:m_timestamp(),
    last_active_time :: nklib_util:m_timestamp(),
    last_seen_msg_time :: nklib_util:m_timestamp(),
    unread_count :: integer(),
    sessions :: [#chat_session{}]
}).


-record(session, {
    members :: [#member{}],
    messages :: [{Time::integer(), MsgId::nkdomain:obj_id(), Msg::map()}]
}).


%% @private
object_info() ->
    #{
        type => ?CHAT_CONVERSATION,
        min_first_time => 5*60*1000,
        dont_create_childs_on_disabled => true,
        dont_update_on_disabled => true
    }.

%% @doc
object_admin_info() ->
    #{
        class => resource,
        weight => 2000
    }.


%% @private
object_es_mapping() ->
    #{
        class => #{type => keyword},
        members => #{
            type => object,
            dynamic => false,
            properties => #{
                member_id => #{type => keyword},
                added_time => #{type => date},
                last_active_time => #{type => date},
                last_seen_message_time => #{type => date}
            }
        }
    }.


%% @private
object_parse(_SrvId, update, _Obj) ->
    #{};

object_parse(_SrvId, load, _Obj) ->
    #{
        class => {atom, [private, channel, one2one]},
        members =>
            {list,
                 #{
                     member_id => binary,
                     added_time => integer,
                     last_active_time => integer,
                     last_seen_message_time => integer,
                     '__mandatory' => [member_id, added_time, last_active_time, last_seen_message_time]
                 }
            },
        '__defaults' => #{class => private, members => []}
    }.


%% @doc
object_create(SrvId, Obj) ->
    #{?CHAT_CONVERSATION := Conv} = Obj,
    Obj2 = Obj#{?CHAT_CONVERSATION => Conv#{members => []}},
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


% @private When the object is loaded, we make our cache
object_init(#?STATE{id=Id, obj=Obj}=State) ->
    #obj_id_ext{obj_id=ConvId} = Id,
    #{?CHAT_CONVERSATION := #{members := MemberList}} = Obj,
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
                last_active_time= LastActiveTime,
                last_seen_msg_time = LastSeenTime,
                unread_count = -1,
                sessions = []
            }
        end,
        MemberList),
    case read_messages(ConvId, State) of
        {ok, Msgs} ->
            Msgs2 = lists:map(
                fun(#{<<"obj_id">>:=MsgId, <<"created_time">>:=Time}=Msg) -> {Time, MsgId, Msg} end,
                Msgs),
            Session = #session{
                members = Members,
                messages = Msgs2
            },
            {ok, State#?STATE{session=Session}};
        {error, Error} ->
            {error, Error}
    end.


%% @private Prepare the object for saving
object_save(#?STATE{obj=Obj, session=Session}=State) ->
    #session{members=Members} = Session,
    MemberList= lists:map(
        fun(Member) ->
            #member{
                member_id = MemberId,
                added_time = Time,
                last_active_time = ActiveTime,
                last_seen_msg_time = MsgTime
            } = Member,
            #{
                member_id => MemberId,
                added_time => Time,
                last_active_time => ActiveTime,
                last_seen_message_time => MsgTime
            }
        end,
        Members),
    Obj2 = ?ADD_TO_OBJ(?CHAT_CONVERSATION, #{members=>MemberList}, Obj),
    {ok, State#?STATE{obj = Obj2}}.


%% @private
object_sync_op({?MODULE, add_member, MemberId}, _From, State) ->
    case add_member(MemberId, State) of
        {ok, State2} ->
            State3 = do_event({member_added, MemberId}, State2),
            State4 = do_event({added_to_conversation, MemberId}, State3),
            {reply_and_save, {ok, MemberId}, State4};
        {error, Error} ->
            {reply, {error, Error}, State}
    end;

object_sync_op({?MODULE, remove_member, MemberId}, _From, State) ->
    case find_member(MemberId, State) of
        {true, #member{sessions=_Sessions}} ->
            % TODO: stop sessions
            State2 = do_event({member_removed, MemberId}, State),              % Using old state
            {ok, State3} = rm_member(MemberId, State2),
            State4 = do_event({removed_from_conversation, MemberId}, State3),
            {reply_and_save, ok, State4};
        false ->
            {reply, {error, member_not_found}, State}
    end;

object_sync_op({?MODULE, add_session, MemberId, SessId, Meta, Pid}, _From, State) ->
    case do_add_session(MemberId, SessId, Meta, Pid, State) of
        {ok, State2} ->
            State3 = do_event({session_added, MemberId, SessId}, State2),
            {reply_and_save, {ok, self()}, State3};
        {error, Error} ->
            {reply, {error, Error}, State}
    end;

object_sync_op({?MODULE, remove_session, UserId, SessId}, _From, State) ->
    case do_rm_session(UserId, SessId, State) of
        {ok, MemberId, State2} ->
            State3 = do_event({session_removed, MemberId, SessId}, State2),
            {reply_and_save, ok, State3};
        {error, Error} ->
            {reply, {error, Error}, State}
    end;

object_sync_op({?MODULE, get_session_info}, _From, #?STATE{id=#obj_id_ext{obj_id=ObjId}}=State) ->
    #?STATE{is_enabled=Enabled, id=#obj_id_ext{path=Path}, obj=Obj} = State,
    Name = maps:get(name, Obj, <<>>),
    #{?CHAT_CONVERSATION:=#{class:=Class}} = Obj,
    MemberIds = maps:keys(get_members(State)),
    Reply = #{
        obj_id => ObjId,
        name => Name,
        path => Path,
        created_by => maps:get(created_by, Obj),
        class => Class,
        description => maps:get(description, Obj, <<>>),
        is_enabled => Enabled,
        member_ids => MemberIds
    },
    {reply, {ok, Reply}, State};

object_sync_op(_Op, _From, _State) ->
    continue.


%% @private
object_async_op({?MODULE, set_active, MemberId, SessId, Bool}, State) ->
    case find_member(MemberId, State) of
        {true, #member{sessions=ChatSessions}=Member} ->
            case lists:keytake(SessId, #chat_session.session_id, ChatSessions) of
                {value, #chat_session{}=ChatSession, ChatSessions2} ->
                    ChatSession2 = ChatSession#chat_session{is_active=Bool},
                    ChatSessions3 = [ChatSession2|ChatSessions2],
                    Member2 = Member#member{sessions=ChatSessions3},
                    Member3 = case Bool of
                        true ->
                            #?STATE{session=#session{messages=Msgs}} = State,
                            Time = case Msgs of
                                [{Time0, _, _}|_] -> Time0;
                                _ -> 0
                            end,
                            send_to_sessions(Member2, {counter_updated, 0}, State),
                            Member2#member{
                                last_active_time = nkdomain_util:timestamp(),
                                last_seen_msg_time = Time,
                                unread_count = 0
                            };
                        false ->
                            Member2
                    end,
                    {noreply, set_member(MemberId, Member3, State)};
                error ->
                    ?LLOG(warning, "set_active for unknown session", [], State),
                    {noreply, State}
            end;
        false ->
            ?LLOG(warning, "set_active for unknown member", [], State),
            {noreply, State}
    end;

object_async_op(_Op, _State) ->
    continue.


%% @private
object_link_down({usage, {?MODULE, session, MemberId, SessId, _Pid}}, State) ->
    {ok, State2} = do_rm_session(MemberId, SessId, State),
    {noreply, State2};

object_link_down(_Link, State) ->
    {ok, State}.

%% @private Hook called before sending a event
object_event({message_created, Msg}, #?STATE{session=Session}=State) ->
    #session{messages=Msgs} = Session,
    #{obj_id:=MsgId, created_time:=Time} = Msg,
    Session2 = Session#session{
        messages = [{Time, MsgId, Msg}|Msgs]
    },
    State2 = do_new_msg_event(Time, Msg, State#?STATE{session=Session2}),
    {ok, State2};

object_event({message_updated, Msg}, #?STATE{session=Session}=State) ->
    #session{messages=Msgs} = Session,
    #{obj_id:=MsgId, created_time:=Time} = Msg,
    Session2 = Session#session{
        messages = lists:keystore(MsgId, 2, Msgs, {Time, MsgId, Msg})
    },
    State2 = do_event_sessions({message_updated, Msg}, State#?STATE{session=Session2}),
    {ok, State2};

object_event({message_deleted, MsgId}, #?STATE{session=Session}=State) ->
    #session{messages=Msgs} = Session,
    Session2 = Session#session{
        messages = lists:keydelete(MsgId, 2, Msgs)
    },
    State2 = do_event_sessions({message_deleted, MsgId}, State#?STATE{session=Session2}),
    {ok, State2};

object_event({member_added, MemberId}, State) ->
    {ok, do_event_sessions({member_added, MemberId}, State)};

object_event({member_removed, MemberId}, State) ->
    {ok, do_event_sessions({member_removed, MemberId}, State)};

object_event(_Event, State) ->
    {ok, State}.




%% ===================================================================
%% Internal
%% ===================================================================


%% @private
find_member(MemberId, State) ->
    Members = get_members(State),
    case lists:keyfind(MemberId, 1, Members) of
        {_, Member} ->
            {true, Member};
        false ->
            false
    end.


%% @private
add_member(MemberId, State) ->
    case find_member(MemberId, State) of
        false ->
            Member = #member{
                added_time = nklib_util:m_timestamp(),
                last_active_time = 0,
                last_seen_msg_time = 0,
                unread_count = -1,
                sessions = []
            },
            {ok, set_member(MemberId, Member, State)};
        {true, _} ->
            {error, member_already_present}
    end.


%% @private
rm_member(MemberId, State) ->
    case find_member(MemberId, State) of
        {true, _} ->
            Members1 = get_members(State),
            Members2 = lists:keydelete(MemberId, 1, Members1),
            {ok, set_members(Members2, State)};
        false ->
            {error, member_not_found}
    end.


%% @private
do_add_session(MemberId, SessId, Meta, Pid, State) ->
    case find_member(MemberId, State) of
        {true, #member{sessions=Sessions}=Member} ->
            Session = #chat_session{meta=Meta, pid=Pid, is_active=false},
            Sessions2 = [Session|Sessions],
            Member2 = Member#member{sessions=Sessions2},
            Member3 = case Member2 of
                #member{last_seen_msg_time=Last, unread_count=-1} ->
                    Count = find_unread(Last, State),
                    send_to_sessions(Member2, {counter_updated, Count}, State),
                    Member2#member{unread_count=Count};
                _ ->
                    Member2
            end,
            State3 = set_member(MemberId, Member3, State),
            State4 = nkdomain_obj:links_add(usage, {?MODULE, session, MemberId, SessId, Pid}, State3),
            {ok, State4};
        false ->
            {error, member_not_found}
    end.


%% @private
do_rm_session(MemberId, SessId, State) ->
    case find_member(MemberId, State) of
        {true, Member} ->
            Sessions1 = maps:get(sessions, Member, #{}),
            case maps:find(SessId, Sessions1) of
                {ok, #{pid:=Pid}} ->
                    State2 = nkdomain_obj:links_remove(usage, {?MODULE, session, MemberId, SessId, Pid}, State),
                    Sessions2 = maps:remove(SessId, Sessions1),
                    Member2 = Member#{sessions=>Sessions2},
                    {ok, set_member(MemberId, Member2, State2)};
                false ->
                    {error, session_not_found}
            end;
        false ->
            {error, session_not_found}
    end.


%% @private
get_members(#?STATE{session=Session}) ->
    #session{members=Members} = Session,
    Members.

%% @private
set_members(Members, #?STATE{session=Session}=State) ->
    Session2 = Session#session{members=Members},
    State#?STATE{session=Session2, is_dirty=true}.

%% @private
set_member(MemberId, Member, State) ->
    Members1 = get_members(State),
    Members2 = lists:keystore(MemberId, 1, Members1, {MemberId, Member}),
    set_members(Members2, State).


%% @private
append_user_data(SrvId, UserId, Data) ->
    case nkdomain_user_obj:get_name(SrvId, UserId) of
        {ok, User} -> Data#{user=>User#{obj_id=>UserId}};
        {error, _} -> Data#{user=>#{}}
    end.


%% @private
do_event(Event, State) ->
    nkdomain_obj_util:event(Event, State).


%% @private
do_event_sessions(Event, State) ->
    Members = get_members(State),
    do_event_sessions(Members, Event, State).


%% @private
do_event_sessions([], _Event, State) ->
    State;

do_event_sessions([{_MemberId, Member}|Rest], Event, State) ->
    #member{sessions=Sessions} = Member,
    send_to_sessions(Sessions, Event, State),
    do_event_sessions(Rest, Event, State).


%% @private
send_to_sessions(Sessions, Event, #?STATE{id=Id}) ->
    #obj_id_ext{obj_id=ConvId} = Id,
    lists:foreach(
        fun(#chat_session{meta=Meta, pid=Pid}) ->
            nkchat_session_obj:conversation_event(Pid, ConvId, Meta, Event)
        end,
        Sessions).


%% @private
do_new_msg_event(Time, Msg, State) ->
    Members = get_members(State),
    do_new_msg_event(Members, Time, Msg, [], State).


%% @private
do_new_msg_event([], _Time, _Msg, Acc, State) ->
    set_members(Acc, State);

do_new_msg_event([Member|Rest], Time, Msg, Acc, State) ->
    #member{
        member_id = MemberId,
        unread_count = Count,
        sessions = Sessions
    } = Member,
    IsActive = lists:keymember(true, #chat_session.is_active, Sessions),
    Acc2 = case Sessions of
        [] ->
            lager:error("NKLOG SEND PUSH TO  ~p", [MemberId]),
            [Member|Acc];
        _ when IsActive ->
            Member2 = Member#member{
                last_seen_msg_time = Time,
                unread_count = 0
            },
            send_to_sessions(Member2, {message_created, Msg}, State),
            send_to_sessions(Member2, {counter_updated, 0}, State),
            [Member2|Acc];
        _ ->
            Count2 = Count + 1,
            Member2 = Member#member{
                unread_count = Count2
            },
            send_to_sessions(Member2, {message_created, Msg}, State),
            send_to_sessions(Member2, {counter_updated, Count2}, State),
            [Member2|Acc]
    end,
    do_new_msg_event(Rest, Time, Msg, Acc2, State).



%% @private
read_messages(ConvId, #?STATE{srv_id=SrvId}) ->
    Search = #{
        filters => #{
            type => ?CHAT_MESSAGE,
            <<?CHAT_MESSAGE/binary, ".conversation_id">> => ConvId
        },
        fields => [created_time, created_by, ?CHAT_MESSAGE],
        sort => <<"desc:created_time">>,
        size => 100
    },
    case nkdomain:search(SrvId, Search) of
        {ok, 0, [], _Meta} ->
            {ok, []};
        {ok, _Num, Objs, _Meta} ->
            {ok, Objs};
        {error, Error} ->
            {error, Error}
    end.


%% @private
find_unread(Time, #?STATE{srv_id=SrvId, id=Id}=State) ->
    #obj_id_ext{obj_id=ConvId} = Id,
    Search = #{
        filters => #{
            type => ?CHAT_MESSAGE,
            domain_id => ConvId,
            created_time => <<">", (integer_to_binary(Time))/binary>>
        },
        size => 0
    },
    case nkdomain:search(SrvId, Search) of
        {ok, Num, [], _Meta} ->
            Num;
        {error, Error} ->
            ?LLOG(error, "error reading unread count: ~p", [Error], State),
            0
    end.


