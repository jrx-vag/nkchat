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

%% @doc Conversation Object Schemas
-module(nkchat_conversation_obj_schema).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([object_execute/5, object_schema/1, object_query/3]).
-export([test_all_conversations/0]).

-include("nkchat.hrl").
-include_lib("nkdomain/include/nkdomain.hrl").

-define(LLOG(Type, Txt, Args),
    lager:Type("NkDOMAIN Conversation "++Txt, Args)).


%% ===================================================================
%% API
%% ===================================================================


%% @doc 
object_execute(Field, _ObjIdExt, Conv, _Args, _Ctx) ->
    case Field of
        <<"conversationType">> -> {ok, maps:get(type, Conv)};
        <<"conversationStatus">> -> {ok, maps:get(status, Conv, null)};
        <<"conversationIsClosed">> -> {ok, maps:get(is_closed, Conv, false)};
        <<"conversationInfo">> -> {ok, maps:get(info, Conv, null)};
        <<"conversationMemberIds">> -> {ok, [{ok, Id} ||  #{member_id:=Id} <- maps:get(members, Conv, [])]};
        <<"conversationMembers">> -> {ok, get_members(Conv)};
        _ -> unknown_field
    end.


%%  @doc Generates new schema entries
object_schema(types) ->
    #{
        'ChatConversation' => #{
            type_class => nkobject,
            fields => #{
                conversationType => {no_null, string},
                conversationStatus => string,
                conversationIsClosed => {no_null, bool},
                conversationInfo => string,
                conversationMemberIds => {list, string},
                conversationMembers => {list, 'ChatConversationMember'}
            },
            comment => "A Chat Conversation"
        },
        'ChatConversationMember' => #{
            fields => #{
                user => {no_null, 'User'},
                addedTime => {no_null, time}
            }
        },
        'ChatConversationConnection' => #{
            type_class => connection
        }
    };

object_schema({connections, 'User'}) ->
    #{
        chatConversation => {connection, 'ChatConversation', #{
            from => int,
            size => int,
            filter => {list, 'ChatConversationFilter'},
            sort => {list, 'ChatConversationSort'}}}
    };

object_schema(inputs) ->
    #{
        'ChatConversationFilter' => #{
            fields => nkdomain_graphql_obj:object_fields_filter(#{
                conversationType => {'FilterKeyword', #{comment => "Conversation type"}},
                conversationStatus => {'FilterKeyword', #{comment => "Conversation status"}},
                conversationIsClosed => 'FilterBoolean',
                conversationHasMemberId => {'FilterKeyword', #{comment => "Conversation has this member"}}
            }),
            comment => "Filter values to sort on"
        },
        'ChatConversationSort' => #{
            fields => nkdomain_graphql_obj:schema_object_fields_sort([conversationType, conversationStatus, conversationIsClosed]),
            comment => "Fields to sort on"
        }
    };

object_schema(queries) ->
    #{
        allChatConversations => nkdomain_graphql_obj:schema_query_all_objs('ChatConversation', 'ChatConversation', 'ChatConversation')
    };


object_schema(_O) ->
    #{}.


%% @doc
%% @doc
object_query({connection, #obj_id_ext{type=?DOMAIN_USER, obj_id=UserObjId}}, Params, _Ctx) ->
    Opts = #{
        fields => #{
            <<"conversationType">> => [?CHAT_CONVERSATION, type],
            <<"conversationStatus">> => [?CHAT_CONVERSATION, status],
            <<"conversationIsClosed">> => [?CHAT_CONVERSATION, is_closed]
        },
        filters => [
            #{<<"conversation.members.member_id">> => #{<<"eq">> => UserObjId}},
            #{<<"type">> => #{<<"eq">> => {enum, <<"ChatConversation">>}}}
        ]
    },
    nkdomain_graphql_util:search(Params, Opts);

object_query(<<"allChatConversations">>, Params, _Ctx) ->
    Opts = #{
        fields => #{
            <<"conversationType">> => [?CHAT_CONVERSATION, type],
            <<"conversationStatus">> => [?CHAT_CONVERSATION, status],
            <<"conversationIsClosed">> => [?CHAT_CONVERSATION, is_closed],
            <<"conversationHasMemberId">> => [?CHAT_CONVERSATION, <<"members.member_id">>]
        },
        filters => [
            #{<<"type">> => #{<<"eq">> => {enum, <<"ChatConversation">>}}}
        ]
    },
    nkdomain_graphql_util:search(Params, Opts).




%% ===================================================================
%% Internal
%% ===================================================================

get_members(#{members:=Members}) ->
    lists:map(
        fun(#{member_id:=MemberId, added_time:=Time}) ->
            case nkdomain_graphql_util:get_obj(MemberId) of
                {ok, User} ->
                    {ok, #{
                        <<"user">> => User,
                        <<"addedTime">> => Time
                    }};
                {error, Error} ->
                    {error, Error}
            end
        end,
        Members).



%% ===================================================================
%% Tests
%% ===================================================================

%% @private
test_all_conversations() ->
    Query =
        <<"query {
            allChatConversations(
                size: 2
                sort: [
                    {path: {order: ASC}},
                    {conversationIsClosed: {order: ASC}}
                ],
                filter: [
                    {conversationIsClosed: {eq: false}}
                ]
              ) {
                totalCount
                objects {
                  path
                  conversationType
                  conversationStatus
                  conversationInfo
                  conversationIsClosed
                  conversationMemberIds
                  conversationMembers {
                    addedTime
                    user {
                      path
                    }
                  }
                }
              }

    }">>,
    request(Query).


request(Query) ->
    nkdomain_graphql:request(?NKROOT, Query, #{}).
