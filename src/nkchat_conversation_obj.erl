%% -------------------------------------------------------------------
%%
%% Copyright (c) 2016 Carlos Gonzalez Florido.  All Rights Reserved.
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
-export([object_get_info/0, object_mapping/0, object_syntax/1,
         object_api_syntax/3, object_api_allow/4, object_api_cmd/4,
         object_init/1, object_sync_op/3, object_handle_info/2]).

-include("nkchat.hrl").
-include_lib("nkdomain/include/nkdomain.hrl").
-include_lib("nkdomain/include/nkdomain_debug.hrl").

-define(CHECK_TIME, 5000).

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
    case nkdomain_obj_lib:load(Srv, Id, #{}) of
        #obj_id_ext{pid=ConvPid} ->
            nkdomain_obj:sync_op(ConvPid, {?MODULE, add_member, MemberId});
        {error, object_not_found} ->
            {error, conversation_not_found};
    {error, Error} ->
            {error, Error}
    end.


%% @doc
-spec remove_member(nkservice:id(), nkdomain:id(), nkdomain:id()) ->
    ok | {error, term()}.

remove_member(Srv, Id, MemberId) ->
    case nkdomain_obj_lib:load(Srv, Id, #{}) of
        #obj_id_ext{pid=ConvPid} ->
            nkdomain_obj:sync_op(ConvPid, {?MODULE, remove_member, MemberId});
        {error, object_not_found} ->
            {error, conversation_not_found};
        {error, Error} ->
            {error, Error}
    end.


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



%% ===================================================================
%% nkdomain_obj behaviour
%% ===================================================================

-record(?MODULE, {
    monitor :: nkdomain_monitor:monitor(),
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
object_init(#obj_session{srv_id=SrvId, obj=Obj}=Session) ->
    #{?CHAT_CONVERSATION := Conv} = Obj,
    MemberIds = maps:get(member_ids, Conv, []),
    Monitor = lists:foldl(
        fun(MemberId, Acc) ->
            case nkdomain_monitor:load_obj(MemberId, Acc) of
                {enabled, Acc2} ->
                    Acc2;
                {disabled, Acc2} ->
                    ?LLOG(notice, "could not active member ~s", [MemberId], Session),
                    Acc2;
                {error, Error} ->
                    ?LLOG(notice, "could not load member ~s: ~p", [MemberId, Error], Session),
                    Acc
            end
        end,
        nkdomain_monitor:new(SrvId, ?MODULE),
        MemberIds),
    self() ! {?MODULE, check_time},
    {ok, Session#obj_session{data=#?MODULE{monitor=Monitor}}}.


%% @private
object_sync_op({?MODULE, add_member, MemberId}, _From, Session) ->
    Monitor1 = get_monitor(Session),
    case nkdomain_monitor:add_obj(MemberId, Monitor1) of
        {ok, ObjId, Monitor2} ->
            {reply_and_save, {ok, ObjId}, update_monitor(Monitor2, Session)};
        {error, Error} ->
            {reply, {error, Error}, Session}
    end;

object_sync_op({?MODULE, remove_member, MemberId}, _From, Session) ->
    Monitor1 = get_monitor(Session),
    case nkdomain_monitor:rm_obj(MemberId, Monitor1) of
        {ok, Monitor2} ->
            {reply_and_save, ok, update_monitor(Monitor2, Session)};
        {error, Error} ->
            {reply, {error, Error}, Session}
    end;

object_sync_op(_Op, _From, _Session) ->
    continue.


%% @private
object_handle_info({?MODULE, check_time}, Session) ->
    Monitor1 = get_monitor(Session),
    Session2 = case nkdomain_monitor:reload_disabled(Monitor1) of
        {[], _} ->
            Session;
        {ObjIds, Monitor2} ->
            ?LLOG(notice, "reloaded objects ~p", [ObjIds], Session),
            update_monitor(Monitor2, Session)
    end,
    erlang:send_after(?CHECK_TIME, self(), {?MODULE, check_time}),
    {noreply, Session2};

object_handle_info({'DOWN', _Ref, process, Pid, _Reason}, Session) ->
    Monitor1 = get_monitor(Session),
    case nkdomain_monitor:down_obj(Pid, Monitor1) of
        {disabled, ObjId, Monitor2} ->
            ?LLOG(notice, "member ~s is disabled", [ObjId], Session),
            {noreply, update_monitor(Monitor2, Session)};
        not_found ->
            continue
    end;

object_handle_info(_Info, _Session) ->
    continue.



%% ===================================================================
%% Internal
%% ===================================================================

%% @private
get_monitor(#obj_session{data=ConvData}) ->
    #?MODULE{monitor=Monitor} = ConvData,
    Monitor.


%% @private
update_monitor(Monitor, #obj_session{obj=Obj, data=ConvData}=Session) ->
    ConvData2 = ConvData#?MODULE{monitor=Monitor},
    #{?CHAT_CONVERSATION := #{member_ids:=ObjIds1}} = Obj,
    case nkdomain_monitor:get_objs(Monitor) of
        ObjIds1 ->
            Session#obj_session{data=ConvData2};
        ObjIds2 ->
            Obj2 = ?ADD_TO_OBJ(?CHAT_CONVERSATION, #{member_ids=>ObjIds2}, Obj),
            Session#obj_session{obj=Obj2, data=ConvData2, is_dirty=true}
    end.
