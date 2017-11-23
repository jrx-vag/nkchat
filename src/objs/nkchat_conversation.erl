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

%% @doc Conversation API

-module(nkchat_conversation).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([create/2]).
-export([add_info/2, add_member/3, remove_member/3]).
-export([add_session/4, set_session_active/4, remove_session/3, get_member_info/2]).
-export([get_status/1, set_status/2, set_closed/2]).
-export([get_info/1, get_messages/2, find_member_conversations/2,
         find_conversations_with_members/2, get_last_messages/1]).
-export([add_invite/2, add_invite_op/4, perform_op/1]).
-export([message_event/2]).
-export([sync_op/2, async_op/2]).

-export_type([event/0]).

-include("nkchat.hrl").
-include_lib("nkdomain/include/nkdomain.hrl").
%%-include_lib("nkdomain/include/nkdomain_debug.hrl").
%%-include_lib("nkservice/include/nkservice.hrl").




%% ===================================================================
%% Types
%% ===================================================================


-type create_opts() ::
    #{
        parent_id => nkdomain:id(),
        created_by => nkdomain:id(),
        obj_name => nkdomain:obj_name(),
        type => binary(),
        obj_name_follows_members => boolean(),
        push_srv_id => atom() | binary(),
        status => status(),
        initial_member_ids => [binary()]
    }.

%-type member_role() :: binary().

-type info() :: map().

-type event() ::
    {added_info, info()} |
    {message_created, nkdomain:obj()} |
    {message_updated, nkdomain:obj()} |
    {message_deleted, nkdomain:obj_id()} |
    {member_added, nkdomain:obj_id()} |
    {added_to_conversation, nkdomain:obj_id()} |        % Same but obj_id is for the member
    {member_removed, nkdomain:obj_id()} |
    {removed_from_conversation, nkdomain:obj_id()} |    % Same but obj_id is for the member
    {session_added, Member::nkdomain:obj_id(), SessId::nkdomain:obj_id()} |
    {session_removed, Member::nkdomain:obj_id(), SessId::nkdomain:obj_id()} |
    {status_updated, status()} |
    {is_closed_updated, boolean()}.


-type status() :: binary().


%% ===================================================================
%% Public
%% ===================================================================


%% @doc

-spec create(nkdomain:id(), create_opts()) ->
    {ok, ConfId::nkdomain:id(), pid()} | {error, term()}.

create(Domain, Opts) ->
    Core = maps:with([created_by, obj_name, name, parent_id], Opts),
    Conv = maps:with([type, status, obj_name_follows_members, push_srv_id, initial_member_ids], Opts),
    Obj = Core#{
        type => ?CHAT_CONVERSATION,
        domain_id => Domain,
        ?CHAT_CONVERSATION => Conv
    },
    case nkdomain_obj_make:create(Obj) of
        {ok, #obj_id_ext{obj_id=ConvId, pid=Pid}, []} ->
            {ok, ConvId, Pid};
        {error, Error} ->
            {error, Error}
    end.


%% @doc
add_info(Id, Info) when is_map(Info) ->
    sync_op(Id, {add_info, Info}).


%% @doc Members will be changed for roles
-spec add_member(nkdomain:id(), nkdomain:id(), map()) ->
    {ok, nkdomain:obj_id()} | {error, term()}.

add_member(Id, Member, Opts) ->
    case nkdomain_lib:find(Member) of
        #obj_id_ext{type= ?DOMAIN_USER, obj_id=MemberId} ->
            case sync_op(Id, {add_member, MemberId}) of
                {ok, ObjId} ->
                    Silent = maps:get(silent, Opts, false),
                    case Silent of
                        false ->
                            Msg = #{
                                type => ?CHAT_MSG_TYPE_ADDED_MEMBER,
                                text => <<"member added">>,
                                created_by => <<"admin">>,
                                body => #{
                                    member_id => MemberId
                                }
                            },
                            case nkchat_message_obj:create(Id, Msg) of
                                {ok, _MsgId, _Pid} ->
                                    ok;
                                {error, Error2} ->
                                    lager:warning("could not add member_added msg to conversation ~s: ~p", [Id, Error2])
                            end;
                        true ->
                            ok
                    end,
                    {ok, ObjId};
                {error, Error} ->
                    {error, Error}
            end;
        {ok, _, _, _} ->
            {error, member_invalid};
        {error, object_not_found} ->
            {error, member_not_found};
        {error, Error} ->
            {error, Error}
    end.


%% @doc
-spec remove_member(nkdomain:id(), nkdomain:id(), map()) ->
    ok | {error, term()}.

remove_member(Id, Member, Opts) ->
    MemberId = case nkdomain_lib:find(Member) of
        #obj_id_ext{obj_id=ObjId} ->
            ObjId;
        _ ->
            Member
    end,
    case sync_op(Id, {remove_member, MemberId}) of
        ok ->
            Silent = maps:get(silent, Opts, false),
            case Silent of
                false ->
                    Msg = #{
                        type => ?CHAT_MSG_TYPE_REMOVED_MEMBER,
                        text => <<"member removed">>,
                        created_by => <<"admin">>,
                        body => #{
                            member_id => MemberId
                        }
                    },
                    case nkchat_message_obj:create(Id, Msg) of
                        {ok, _MsgId, _Pid} ->
                            ok;
                        {error, Error2} ->
                            lager:warning("could not add member_removed msg to conversation ~s: ~p", [Id, Error2])
                    end;
                true ->
                    ok
                end;
        {error, Error} ->
            {error, Error}
    end.


%% @doc
-spec get_status(nkdomain:id()) ->
    {ConvId::nkdomain:obj_id(), DomainId::nkdomain:obj_id(), Status::status(), IsClosed::boolean()}.

get_status(Id) ->
    sync_op(Id, {get_status}).


%% @doc
-spec set_status(nkdomain:id(), status()) ->
    ok | {error, term()}.

set_status(Id, Status) ->
    sync_op(Id, {set_status, nklib_util:to_binary(Status)}).


%% @doc
-spec set_closed(nkdomain:id(), boolean()) ->
    ok | {error, term()}.

set_closed(Id, Closed) when is_boolean(Closed) ->
    sync_op(Id, {set_closed, Closed}).


%% @private Called from nkchat_session_obj
%% Sessions receive notifications for every message, calling
-spec add_session(nkdomain:obj_id(), nkdomain:obj_id(), nkdomain:obj_id(), map()) ->
    {ok, pid()} | {error, term()}.

add_session(ConvId, MemberId, SessId, Meta) ->
    sync_op(ConvId, {add_session, MemberId, SessId, Meta, self()}).


%% @private
set_session_active(ConvId, MemberId, SessId, Bool) when is_boolean(Bool)->
    async_op(ConvId, {set_active, MemberId, SessId, Bool}).


%% @private Called from nkchat_session_obj
%% Sessions receive notifications for every message, calling
remove_session(ConvId, MemberId, SessId) ->
    sync_op(ConvId, {remove_session, MemberId, SessId}).


%% @private
-spec get_member_info(nkdomain:obj_id(), nkdomain:obj_id()) ->
    {ok, map()} | {error, term()}.

get_member_info(ConvId, MemberId) ->
    sync_op(ConvId, {get_member_info, MemberId}).


%% @private
get_info(Pid) ->
    sync_op(Pid, {get_info}).


%% @doc
find_member_conversations(Domain, MemberId) ->
    case nkdomain_lib:find(Domain) of
        #obj_id_ext{type=?DOMAIN_DOMAIN, obj_id=DomainId} ->
            Filters = #{
                type => ?CHAT_CONVERSATION,
                domain_id => DomainId,
                << ?CHAT_CONVERSATION/binary, ".members.member_id">> => MemberId
            },
            Search2 = #{
                fields => [<<?CHAT_CONVERSATION/binary, ".type">>],
                filters => Filters,
                size => 9999
            },
            case nkdomain:search(Search2) of
                {ok, _N, List, _Meta} ->
                    List2 = lists:map(
                        fun(#{<<"obj_id">>:=ConvId, ?CHAT_CONVERSATION:=#{<<"type">>:=Type}}) -> {ConvId, Type} end,
                        List),
                    {ok, List2};
                {error, Error} ->
                    {error, Error}
            end;
        _ ->
            {error, domain_unknown}
    end.


%% @doc
find_conversations_with_members(Domain, MemberIds) ->
    case nkdomain_lib:find(Domain) of
        #obj_id_ext{type=?DOMAIN_DOMAIN, obj_id=DomainId} ->
            Hash = nkchat_conversation_obj:make_members_hash(MemberIds),
            Filters = #{
                type => ?CHAT_CONVERSATION,
                domain_id => DomainId,
                << ?CHAT_CONVERSATION/binary, ".members_hash">> => Hash
            },
            Search2 = #{
                fields => [<<?CHAT_CONVERSATION/binary, ".type">>],
                filters => Filters,
                size => 9999
            },
            case nkdomain:search(Search2) of
                {ok, N, List, _Meta} ->
                    List2 = lists:map(
                        fun(#{<<"obj_id">>:=ConvId, ?CHAT_CONVERSATION:=#{<<"type">>:=Type}}) -> {ConvId, Type} end,
                        List),
                    {ok, N, List2};
                {error, Error} ->
                    {error, Error}
            end;
        _ ->
            {error, domain_unknown}
    end.


%% @doc
get_messages(Id, Spec) ->
    case nkdomain_lib:load(Id) of
        #obj_id_ext{obj_id=ConvId} ->
            Search1 = case Spec of
                #{start_date:=_, end_date:=_} ->
                    #{};
                #{start_date:=_} ->
                    maps:with([size], Spec);
                #{end_date:=_} ->
                    maps:with([size], Spec);
                _ ->
                    maps:with([from, size], Spec)
            end,
            Filters1 = #{
                type => ?CHAT_MESSAGE,
                parent_id => ConvId
            },
            {Order, CreatedFilterList} = case Spec of
                #{start_date:=Date1, end_date:=Date2, inclusive:=Inc} when Inc == true ->
                    {desc, ["<", nklib_util:to_binary(Date1), "-", nklib_util:to_binary(Date2), ">"]};
                #{start_date:=Date1, end_date:=Date2} ->
                    {desc, ["<<", nklib_util:to_binary(Date1), "-", nklib_util:to_binary(Date2), ">>"]};
                #{start_date:=Date, inclusive:=Inc} when Inc == true ->
                    {asc, [">=", nklib_util:to_binary(Date)]};
                #{start_date:=Date} ->
                    {asc, [">", nklib_util:to_binary(Date)]};
                #{end_date:=Date, inclusive:=Inc} when Inc == true ->
                    {desc, ["<=", nklib_util:to_binary(Date)]};
                #{end_date:=Date} ->
                    {desc, ["<", nklib_util:to_binary(Date)]};
                _ ->
                    {desc, []}
            end,
            Filters2 = case CreatedFilterList of
                [] ->
                    Filters1;
                _ ->
                    Filters1#{created_time => list_to_binary(CreatedFilterList)}
            end,
            Search2 = Search1#{
                sort => [#{created_time => #{order => Order}}],
                fields => [created_time, ?CHAT_MESSAGE, created_by],
                filters => Filters2
            },
            case nkdomain:search(Search2) of
                {ok, N, List, _Meta} ->
                    List2 = case Order of
                        asc ->
                            lists:reverse(List);
                        desc ->
                            List
                    end,
                    {ok, #{total=>N, data=>List2}};
                {error, Error} ->
                    {error, Error}
            end;
        {error, object_not_found} ->
            {error, conversation_not_found};
        {error, Error} ->
            {error, Error}
    end.


%% @doc
get_last_messages(Id) ->
    sync_op(Id, {get_last_messages}).


%% @private Called from nkchat_message_obj
%% The message object generates an event that inserts into the conversation object
%% and it sends it as if it were an own message
%% Is is captured at object_event/2 and sent to the sessions in send_to_sessions/3
message_event(ConvId, Event) ->
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
            nkdomain_obj:async_op(ConvId, {send_event, Event2})
    end.


%% @doc Adds info on a token to invite an user
-spec add_invite_op(nkdomain:id(), nkdomain:id(), nkdomain:id(), map()) ->
    {ok, ConvId::nkdomain:obj_id(), MemberId::nkdomain:obj_id(), UserId::nkdomain:obj_id(), map()} | {error, term()}.

add_invite_op(Conv, UserId, Member, Base) ->
    sync_op(Conv, {add_invite_op, UserId, Member, Base}).


%% @doc
perform_op(#{?CHAT_CONVERSATION:=#{<<"add_member_op">>:=Op}}) ->
    #{
        <<"conversation_id">> := ConvId,
        <<"member_id">> := MemberId,
        <<"user_id">> := _UserId
    } = Op,
    case add_member(ConvId, MemberId, #{silent => false}) of
        {ok, _MemberId} ->
            ok;
        {error, Error} ->
            {error, Error}
    end;

perform_op(_Data) ->
    {error, operation_token_invalid}.


%% @doc Adds a new invitation token
- spec add_invite(nkdomain:id(), nkdomain:id()) ->
    ok | {error, term()}.

add_invite(Conv, TokenId) ->
    async_op(Conv, {add_invite, TokenId}).


%% @private
sync_op(Conv, Op) ->
    nkdomain_obj:sync_op(Conv, {nkchat_conversation_obj, Op}).


%% @private
async_op(Conv, Op) ->
    nkdomain_obj:async_op(Conv, {nkchat_conversation_obj, Op}).
