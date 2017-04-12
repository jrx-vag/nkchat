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

-module(nkchat_conversation_obj).
-behavior(nkdomain_obj).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([create/4, add_member/3, remove_member/3, get_messages/3]).
-export([message_created/3, message_deleted/2, message_updated/3]).
-export([register_session/3]).
-export([object_get_info/0, object_mapping/0, object_syntax/1,
         object_api_syntax/3, object_api_allow/4, object_api_cmd/4,
         object_start/1, object_sync_op/3, object_async_op/2, object_handle_info/2]).

-include("nkchat.hrl").
-include_lib("nkdomain/include/nkdomain.hrl").
-include_lib("nkdomain/include/nkdomain_debug.hrl").
-include_lib("nkservice/include/nkservice.hrl").


%% Period to find for inactive users
-define(CHECK_TIME, 5*60*1000).

%% Conversation creates events with class = ?CHAT_CONVERSATION, obj_id = ConvId
%%
%% - Subclass: <<"message">>
%%
%%   - Type: <<"created">>
%%     Body: message_id, created_time, text
%%
%%   - Type: <<"deleted">>
%%     Body: message_id
%%
%%   - Type: <<"updated">>
%%     Body: message_id, updated_time, text


%% ===================================================================
%% Types
%% ===================================================================


%% @doc
%% Data must follow object's syntax
-spec create(nkservice:id(), nkdomain:id(), nkdomain:name(), binary()) ->
    {ok, nkdomain:obj_id(), nkdomain:path(), pid()} | {error, term()}.

create(Srv, Domain, Name, Desc) ->
    Opts = #{
        name => Name,
        description => Desc,
        type_obj => #{member_ids => []}
    },
    nkdomain_obj_lib:make_and_create(Srv, Domain, ?CHAT_CONVERSATION, Opts).


%% @doc
-spec add_member(nkservice:id(), nkdomain:id(), nkdomain:id()) ->
    {ok, nkdomain:obj_id()} | {error, term()}.

add_member(Srv, Id, MemberId) ->
    sync_op(Srv, Id, {?MODULE, add_member, MemberId}).


%% @doc
-spec remove_member(nkservice:id(), nkdomain:id(), nkdomain:id()) ->
    ok | {error, term()}.

remove_member(Srv, Id, MemberId) ->
    sync_op(Srv, Id, {?MODULE, remove_member, MemberId}).


%% @doc
get_messages(Srv, Id, Spec) ->
    case nkdomain_obj_lib:load(Srv, Id, #{}) of
        #obj_id_ext{obj_id=ConvId} ->
            Search1 = maps:with([from, size], Spec),
            Filters1 = #{parent_id => ConvId},
            Filters2 = case Spec of
                #{start_date:=Date} ->
                    Filters1#{created_time => {Date, none}};
                _ ->
                    Filters1
            end,
            Search2 = Search1#{
                sort => [#{created_time => #{order => desc}}],
                fields => [created_time, ?CHAT_MESSAGE],
                filters => Filters2
            },

            case nkdomain_store:find(Srv, Search2) of
                {ok, N, List, _Meta} ->
                    {ok, #{total=>N, data=>List}};
                {error, Error} ->
                    {error, Error}
            end;
        {error, object_not_found} ->
            {error, conversation_not_found};
        {error, Error} ->
            {error, Error}
    end.


%% @private Called from nkchat_session_obj
%% Adds the user and register the session
register_session(ConvPid, UserId, Link) ->
    nkdomain_obj:sync_op(ConvPid, {?MODULE, register_session, UserId, Link}).


%% @private Called from nkchat_message_obj
message_created(ConvPid, MsgId, Msg) ->
    nkdomain_obj:async_op(ConvPid, {?MODULE, message_created, MsgId, Msg}).


%% @private
message_deleted(ConvPid, MsgId) ->
    nkdomain_obj:async_op(ConvPid, {?MODULE, message_deleted, MsgId}).


%% @private
message_updated(ConvPid, MsgId, Msg) ->
    nkdomain_obj:async_op(ConvPid, {?MODULE, message_updated, MsgId, Msg}).




%% ===================================================================
%% nkdomain_obj behaviour
%% ===================================================================

-record(?MODULE, {
    sessions = #{} :: #{nkdomain:obj_id() => nklib:link()},
    session_pids = #{} :: #{pid() => nkdomain:obj_id()},
    meta = #{} :: map()
}).


%% @private
object_get_info() ->
    #{
        type => ?CHAT_CONVERSATION,
        min_first_time => 5*60*1000,
        dont_create_childs_on_disabled => true,
        dont_update_on_disabled => true
    }.


%% @private
object_mapping() ->
    #{
        member_ids => #{type => keyword}
    }.


%% @private
object_syntax(_) ->
    #{
        member_ids => {list, binary}
    }.


%% @private
object_api_syntax(Sub, Cmd, Syntax) ->
    nkchat_conversation_obj_syntax:api(Sub, Cmd, Syntax).


%% @private
object_api_allow(_Sub, _Cmd, _Data, State) ->
    {true, State}.


%% @private
object_api_cmd(Sub, Cmd, Data, State) ->
    nkchat_conversation_obj_api:cmd(Sub, Cmd, Data, State).


%% @private When the object is loaded, we make our cache
object_start(Session) ->
    Data = #?MODULE{},
    {ok, Session#obj_session{data=Data}}.


%% @private
object_sync_op({?MODULE, add_member, Id}, _From, Session) ->
    case add_member(Id, Session) of
        {ok, UserId, Session2} ->
            {reply_and_save, {ok, UserId}, Session2};
        {error, Error} ->
            {reply, {error, Error}, Session}
    end;

object_sync_op({?MODULE, remove_member, Id}, _From, Session) ->
    case rm_member(Id, Session) of
        {ok, Session2} ->
            {reply_and_save, ok, Session2};
        {error, Error} ->
            {reply, {error, Error}, Session}
    end;

object_sync_op({?MODULE, register_session, UserId, Link}, _From, Session) ->
    Session2 = do_register_session(UserId, Link, Session),
    {reply, ok, Session2};

object_sync_op(_Op, _From, _Session) ->
    continue.


%% @private
object_async_op({?MODULE, message_created, MsgId, Msg}, Session) ->
    ?DEBUG("message ~s created", [MsgId], Session),
    Event = make_event(message, created, Msg#{message_id=>MsgId}, Session),
    send_event(Event, with_push, Session),
    {noreply, Session};

object_async_op({?MODULE, message_deleted, MsgId}, Session) ->
    ?DEBUG("message ~s deleted", [MsgId], Session),
    Event = make_event(message, deleted, #{message_id=>MsgId}, Session),
    send_event(Event, without_push, Session),
    {noreply, Session};

object_async_op({?MODULE, message_updated, MsgId, Msg}, Session) ->
    ?DEBUG("message ~s updated", [MsgId], Session),
    Event = make_event(message, deleted, Msg#{message_id=>MsgId}, Session),
    send_event(Event, without_push, Session),
    {noreply, Session};

object_async_op(_Op, _Session) ->
    continue.


%% @private
object_handle_info({'DOWN', _Ref, process, Pid, _Reason}, Session) ->
    #obj_session{data=#?MODULE{session_pids=Pids, sessions=Sessions}=Data} = Session,
    case maps:find(Pid, Pids) of
        {ok, UserId} ->
            Sessions2 = maps:remove(UserId, Sessions),
            Pids2 = maps:remove(Pid, Pids),
            Data2 = Data#?MODULE{sessions=Sessions2, session_pids=Pids2},
            Session2 = Session#obj_session{data=Data2},
            {noreply, Session2};
        error ->
            continue
    end;

object_handle_info(_Info, _Session) ->
    continue.



%% ===================================================================
%% Internal
%% ===================================================================

%% @private
sync_op(Srv, Id, Op) ->
    nkdomain_obj_lib:sync_op(Srv, Id, ?CHAT_CONVERSATION, Op, conversation_not_found).


%%%% @private
%%async_op(Srv, Id, Op) ->
%%    nkdomain_obj_lib:async_op(Srv, Id, ?CHAT_CONVERSATION, Op, conversation_not_found).


%% @private
find_member(Id, #obj_session{srv_id=SrvId}=Session) ->
    MemberIds = get_members(Session),
    case lists:member(Id, MemberIds) of
        true ->
            {true, Id};
        false ->
            case nkdomain_obj_lib:find(SrvId, Id) of
                #obj_id_ext{obj_id=ObjId} ->
                    case lists:member(ObjId, MemberIds) of
                        true ->
                            {true, ObjId};
                        false ->
                            {false, ObjId}
                    end;
                {error, Error} ->
                    {error, Error}
            end
    end.



%% @private
add_member(Id, Session) ->
    case find_member(Id, Session) of
        {false, MemberId} ->
            MemberIds = get_members(Session),
            Session2 = set_members([MemberId|MemberIds], Session),
            send_event({started_member, MemberId}, without_push, Session),
            {ok, MemberId, Session2};
        {true, _} ->
            {error, member_already_present};
        {error, object_not_found} ->
            {error, member_not_found};
        {error, Error} ->
            {error, Error}
    end.


%% @private
rm_member(Id, Session) ->
    case find_member(Id, Session) of
        {true, MemberId} ->
            MemberIds = get_members(Session),
            Session2 = set_members(MemberIds -- [MemberId], Session),
            send_event({removed_member, MemberId}, without_push, Session),
            {ok, Session2};
        {false, _} ->
            {error, member_not_found};
        {error, Error} ->
            {error, Error}
    end.


%% @private
do_register_session(MemberId, Link, #obj_session{data=Data}=Session) ->
    #?MODULE{sessions=Sessions, session_pids=Pids} = Data,
    Pid = nklib_links:get_pid(Link),
    case maps:is_key(Pid, Pids) of
        true ->
            Session;
        false ->
            monitor(process, Pid),
            Sessions2 = Sessions#{MemberId => Link},
            Pids2 = Pids#{Pid => MemberId},
            Data2 = Data#?MODULE{sessions=Sessions2, session_pids=Pids2},
            Session#obj_session{data=Data2}
    end.


%% @private
get_members(#obj_session{obj=Obj}) ->
    #{?CHAT_CONVERSATION:=#{member_ids:=MemberIds}} = Obj,
    MemberIds.


%% @private
set_members(MemberIds, #obj_session{obj=Obj}=Session) ->
    #{?CHAT_CONVERSATION:=ChatConv} = Obj,
    ChatConv2 = ChatConv#{member_ids=>MemberIds},
    Obj2 = ?ADD_TO_OBJ(?CHAT_CONVERSATION, ChatConv2, Obj),
    Session#obj_session{obj=Obj2, is_dirty=true}.


%% @private
send_event(Event, Push, #obj_session{data=Data}=Session) ->
    MemberIds = get_members(Session),
    #?MODULE{sessions=Sessions} = Data,
    send_event_sessions(maps:to_list(Sessions), Event, Push, Session),
    case Push of
        with_push ->
            send_event_members(MemberIds, Event, Sessions, Session);
        without_push ->
            ok
    end.


%% @private
send_event_sessions([], _Event, _Push, _Session) ->
    ok;

send_event_sessions([{MemberId, Link}|Rest], Event, Push, #obj_session{srv_id=SrvId}=Session) ->
    case SrvId:object_session_event(Link, Event) of
        ok ->
            ok;
        {error, Error} ->
            ?LLOG(notice, "error sending message to session ~s: ~p", [MemberId, Error], Session),
            case Push of
                with_push ->
                    send_push(MemberId, Event, Session);
                without_push ->
                    ok
            end
    end,
    send_event_sessions(Rest, Event, Push, Session).


%% @private
send_event_members([], _Event, _Sessions, _Session) ->
    ok;

send_event_members([MemberId|Rest], Event, Sessions, Session) ->
    case maps:is_key(MemberId, Sessions) of
        true ->
            ok;
        false ->
            send_push(MemberId, Event, Session)
    end,
    send_event_members(Rest, Event, Sessions, Session).


%% @private
send_push(MemberId, Event, #obj_session{srv_id=SrvId}) ->
    case SrvId:object_member_event(SrvId, MemberId, Event) of
        ok ->
            ok;
        {error, Error} ->
            ?LLOG(notice, "error sending push to ~s: ~p", [MemberId], Error)
    end.


%% @private
make_event(Sub, Type, Body, #obj_session{srv_id=SrvId, obj_id=ObjId}) ->
    #event{
        srv_id = SrvId,
        class = ?CHAT_CONVERSATION,
        subclass = nklib_util:to_binary(Sub),
        type = nklib_util:to_binary(Type),
        obj_id = ObjId,
        body = Body
    }.

