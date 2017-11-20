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

%% @doc Message Object Schemas
-module(nkchat_message_obj_schema).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([object_execute/5, object_schema/1, object_query/3]).
-export([test_all_conversations/0]).

-include("nkchat.hrl").
-include_lib("nkdomain/include/nkdomain.hrl").

-define(LLOG(Type, Txt, Args),
    lager:Type("NkDOMAIN Message "++Txt, Args)).


%% ===================================================================
%% API
%% ===================================================================


%% @doc 
object_execute(Field, _ObjIdExt, Msg, _Args, _Ctx) ->
    case Field of
        <<"messageType">> -> {ok, maps:get(type, Msg, null)};
        <<"messageText">> -> {ok, maps:get(text, Msg, null)};
        <<"messageLayout">> -> {ok, maps:get(layout, Msg, null)};
        <<"messageFileId">> -> {ok, maps:get(file_id, Msg, null)};
        <<"messageFile">> -> nkdomain_graphql_util:get_obj(maps:get(file_id, Msg, <<>>));
        <<"messageBody">> -> {ok, maps:get(body, Msg, null)};
        _ -> unknown_field
    end.


%%  @doc Generates new schema entries
object_schema(types) ->
    #{
        'ChatMessage' => #{
            type_class => nkobject,
            fields => #{
                messageType => string,
                messageText => string,
                messageLayout => string,
                messageFileId => string,
                messageFile => 'File',
                messageBody => string
            },
            comment => "A Chat Message"
        },
        'ChatMessageConnection' => #{
            type_class => connection
        }
    };

object_schema({connections, 'User'}) ->
    #{
        chatMessage => {connection, 'ChatMessage', #{
            from => int,
            size => int,
            filter => {list, 'ChatMessageFilter'},
            sort => {list, 'ChatMessageSort'}}}
    };

object_schema({connections, 'ChatConversation'}) ->
    #{
        chatMessage => {connection, 'ChatMessage', #{
            from => int,
            size => int,
            filter => {list, 'ChatMessageFilter'},
            sort => {list, 'ChatMessageSort'}}}
    };

object_schema(inputs) ->
    #{
        'ChatMessageFilter' => #{
            fields => nkdomain_graphql_obj:object_fields_filter(#{
                messageType => {'FilterKeyword', #{comment => "Message type"}},
                messageText => {'FilterNormalizedString', #{comment => "Message text"}}
            }),
            comment => "Filter values to sort on"
        },
        'ChatMessageSort' => #{
            fields => nkdomain_graphql_obj:schema_object_fields_sort([messageType]),
            comment => "Fields to sort on"
        }
    };

object_schema(queries) ->
    #{
        allChatMessages => nkdomain_graphql_obj:schema_query_all_objs('ChatMessage', 'ChatMessage', 'ChatMessage')
    };


object_schema(_O) ->
    #{}.


%% @doc
object_query({connection, #obj_id_ext{type=?DOMAIN_USER, obj_id=UserObjId}}, Params, _Ctx) ->
    Opts = #{
        fields => #{
            <<"messageType">> => [?CHAT_CONVERSATION, type],
            <<"messageText">> => [?CHAT_CONVERSATION, text]
        },
        filters => [
            #{<<"created_by">> => #{<<"eq">> => UserObjId}},
            #{<<"type">> => #{<<"eq">> => {enum, <<"ChatMessage">>}}}
        ]
    },
    nkdomain_graphql_util:search(Params, Opts);

object_query({connection, #obj_id_ext{type=?CHAT_CONVERSATION, obj_id=ConvObjId}}, Params, _Ctx) ->
    Opts = #{
        fields => #{
            <<"messageType">> => [?CHAT_CONVERSATION, type],
            <<"messageText">> => [?CHAT_CONVERSATION, text]
        },
        filters => [
            #{<<"parent_id">> => #{<<"eq">> => ConvObjId}},
            #{<<"type">> => #{<<"eq">> => {enum, <<"ChatMessage">>}}}
        ]
    },
    nkdomain_graphql_util:search(Params, Opts);

object_query(<<"allChatMessages">>, Params, _Ctx) ->
    Opts = #{
        fields => #{
            <<"messageType">> => [?CHAT_CONVERSATION, type],
            <<"messageText">> => [?CHAT_CONVERSATION, text]
        },
        filters => [
            #{<<"type">> => #{<<"eq">> => {enum, <<"ChatMessage">>}}}
        ]
    },
    nkdomain_graphql_util:search(Params, Opts).




%% ===================================================================
%% Internal
%% ===================================================================



%% ===================================================================
%% Tests
%% ===================================================================

%% @private
test_all_conversations() ->
    Query =
        <<"query {
            allChatMessages(
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
