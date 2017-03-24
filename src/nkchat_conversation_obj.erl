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

-export([create/4, add_members/3, remove_members/3]).
-export([object_get_info/0, object_mapping/0, object_syntax/1,
         object_api_syntax/3, object_api_allow/4, object_api_cmd/4]).

-include("nkchat.hrl").
-include_lib("nkdomain/include/nkdomain.hrl").


%% ===================================================================
%% Types
%% ===================================================================


%% ===================================================================
%% API
%% ===================================================================

%% @doc
%% Data must follow object's syntax
-spec create(nkservice:id(), nkdomain:name(), nkdomain:id(), binary()) ->
    {ok, nkdomain:obj_id(), nkdomain:path(), pid()} | {error, term()}.

create(Srv, Name, Domain, Desc) ->
    Opts = #{father=>Domain, name=>Name},
    Base = #{
        description => Desc,
        ?CHAT_CONVERSATION => #{
            member_ids => []
        }
    },
    case nkdomain_obj_lib:make_obj(Srv, ?CHAT_CONVERSATION, Base, Opts) of
        {ok, Obj} ->
            lager:error("CREATE: ~p", [Obj]),
            case nkdomain:create(Srv, Obj, #{}) of
                {ok, ?CHAT_CONVERSATION, ObjId, Path, Pid} ->
                    {ok, ObjId, Path, Pid};
                {error, Error} ->
                    {error, Error}
            end;        {error, Error} ->
            {error, Error}
    end.

%% @doc
-spec add_members(nkservice:id(), nkdomain:id(), [nkdomain:obj_id()]) ->
    {ok, map()} | {error, term()}.

add_members(Srv, Id, MemberIds) ->
    Fun = fun(#obj_session{obj_id=ObjId, obj=Obj}=Session) ->
        case Obj of
            #{?CHAT_CONVERSATION:=#{member_ids:=MemberIds0}=Conv} ->
                MemberIds2 = lists:usort(MemberIds++MemberIds0),
                Conv2 = Conv#{member_ids:=MemberIds2},
                Obj2 = ?ADD_TO_OBJ(?CHAT_CONVERSATION, Conv2, Obj),
                {ok, #{}, Session#obj_session{obj=Obj2, is_dirty=true}};
            _ ->
                {error, invalid_object}
        end
    end,
    nkdomain_util:update(Srv, ?CHAT_CONVERSATION, Id, Fun).


%% @doc
-spec remove_members(nkservice:id(), nkdomain:id(), [nkdomain:obj_id()]) ->
    {ok, map()} | {error, term()}.

remove_members(Srv, Id, MemberIds) ->
    Fun = fun(#obj_session{obj_id=ObjId, obj=Obj}=Session) ->
        case Obj of
            #{?CHAT_CONVERSATION:=#{member_ids:=MemberIds0}=Conv} ->
                MemberIds2 = MemberIds0 -- MemberIds,
                Conv2 = Conv#{member_ids:=MemberIds2},
                Obj2 = ?ADD_TO_OBJ(?CHAT_CONVERSATION, Conv2, Obj),
                {ok, #{}, Session#obj_session{obj=Obj2, is_dirty=true}};
            _ ->
                {error, invalid_object}
        end
    end,
    nkdomain_util:update(Srv, ?CHAT_CONVERSATION, Id, Fun).


%% ===================================================================
%% nkdomain_obj behaviour
%% ===================================================================


%% @private
object_get_info() ->
    #{
        type => ?CHAT_CONVERSATION
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




%% ===================================================================
%% Internal
%% ===================================================================
