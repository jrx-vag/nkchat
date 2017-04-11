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
-export([message_created/4, message_deleted/2, message_updated/3]).
-export([register_session/3]).
-export([object_get_info/0, object_mapping/0, object_syntax/1,
         object_api_syntax/3, object_api_allow/4, object_api_cmd/4,
         object_start/1, object_sync_op/3, object_async_op/2, object_handle_info/2]).

-include("nkchat.hrl").
-include_lib("nkdomain/include/nkdomain.hrl").
-include_lib("nkdomain/include/nkdomain_debug.hrl").

%% Period to find for inactive users
-define(CHECK_TIME, 5*60*1000).

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
                fields => [created_time, 'chat.message.message'],
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
message_created(ConvPid, MsgId, Time, Msg) ->
    nkdomain_obj:async_op(ConvPid, {?MODULE, message_created, MsgId, Time, Msg}).


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
    sessions = #{} :: #{pid() => nkdomain:obj_id(), nkdomain:obj_id() => pid()},
    meta = #{} :: map()
}).


%% @private
object_get_info() ->
    #{
        type => ?CHAT_CONVERSATION,
        min_first_time => 5*60*1000
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
object_async_op({?MODULE, message_created, MsgId, Time, Msg}, Session) ->
    ?LLOG(notice, "message ~s created", [MsgId], Session),
    send_msg({message, MsgId, {created, Time, Msg}}, with_push, Session),
    {noreply, Session};

object_async_op({?MODULE, message_deleted, MsgId}, Session) ->
    ?LLOG(notice, "message ~s deleted", [MsgId], Session),
    send_msg({message, MsgId, deleted}, without_push, Session),
    {noreply, Session};

object_async_op({?MODULE, message_updated, MsgId, Msg}, Session) ->
    ?LLOG(notice, "message ~s updated", [MsgId], Session),
    send_msg({message, MsgId, {updated, Msg}}, without_push, Session),
    {noreply, Session};

object_async_op(_Op, _Session) ->
    continue.


%% @private
object_handle_info({'DOWN', _Ref, process, Pid, _Reason}, Session) ->
    #obj_session{data=#?MODULE{sessions=Sessions}=Data} = Session,
    case maps:find(Pid, Sessions) of
        {ok, UserId} ->
            Sessions2 = maps:remove(UserId, Sessions),
            Sessions3 = maps:remove(Pid, Sessions2),
            Data2 = Data#?MODULE{sessions=Sessions3},
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
            send_msg({started_member, MemberId}, without_push, Session),
            {ok, MemberId, Session2};
        {true, _} ->
            {error, member_already_present};
        {error, Error} ->
            {error, Error}
    end.


%% @private
rm_member(Id, Session) ->
    case find_member(Id, Session) of
        {true, MemberId} ->
            MemberIds = get_members(Session),
            Session2 = set_members(MemberIds -- [MemberId], Session),
            send_msg({removed_member, MemberId}, without_push, Session),
            {ok, Session2};
        {false, _} ->
            {error, member_not_found};
        {error, Error} ->
            {error, Error}
    end.


%% @private
do_register_session(MemberId, Link, #obj_session{data=Data}=Session) ->
    #?MODULE{sessions=Sessions} = Data,
    Pid = nklib_links:get_pid(Link),
    case maps:is_key(Pid, Sessions) of
        true ->
            Session;
        false ->
            monitor(process, Pid),
            Sessions2 = Sessions#{
                MemberId => Link,
                Pid => MemberId
            },
            Data2 = Data#?MODULE{sessions=Sessions2},
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
send_msg(Msg, Push, #obj_session{data=Data}=Session) ->
    MemberIds = get_members(Session),
    #?MODULE{sessions=Sessions} = Data,
    send_msg_sessions(maps:to_list(Sessions), Push, Msg, Session),
    case Push of
        with_push ->
            send_msg_members(MemberIds, Msg, Sessions, Session);
        without_push ->
            ok
    end.


%% @private
send_msg_sessions([], _Msg, _Push, _Session) ->
    ok;

send_msg_sessions([{MemberId, Link}|Rest], Msg, Push, #obj_session{srv_id=SrvId, obj_id=ConvId}=Session) ->
    case SrvId:object_session_msg(Link, ?CHAT_CONVERSATION, ConvId, Msg) of
        ok ->
            ok;
        {error, Error} ->
            ?LLOG(notice, "error sending message to session ~s: ~p", [MemberId, Error], Session),
            case Push of
                with_push -> send_push(MemberId, Msg, Session);
                without_push -> ok
            end
    end,
    send_msg_sessions(Rest, Msg, Push, Session).



%% @private
send_msg_members([], _Msg, _Sessions, _Session) ->
    ok;

send_msg_members([MemberId|Rest], Msg, Sessions, Session) ->
    case maps:is_key(MemberId, Sessions) of
        true ->
            ok;
        false ->
        send_push(MemberId, Msg, Session)
    end,
    send_msg_members(Rest, Msg, Sessions, Session).


%% @private
send_push(MemberId, Msg, #obj_session{srv_id=SrvId, obj_id=ConvId}) ->
    case SrvId:object_send_push(SrvId, MemberId, ?CHAT_CONVERSATION, ConvId, Msg) of
        ok ->
            ok;
        {error, Error} ->
            ?LLOG(notice, "error sending push to ~s: ~p", [MemberId], Error)
    end.




