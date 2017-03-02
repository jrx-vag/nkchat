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

-module(nkchat_es).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([create_indices/1, remove_index/2]).
-export([read_user/2, write_user/3, delete_user/2, list_users/2,
         delete_all_users/1]).
-export([read_conversation/2, write_conversation/3,
    delete_all_conversations/1, list_conversations/2, delete_conversation/2]).
-export([read_message/2, write_message/3,
    delete_all_messages/1, list_messages/2, delete_message/2]).

-define(LLOG(Type, Txt, Args), lager:Type("Chat Store: "++Txt, Args)).

-define(ES_USER_ALIAS, "chat_user").
-define(ES_USER_INDEX, "chat_user_v1").
-define(ES_USER_TYPE, "user").

-define(ES_CONV_ALIAS, "chat_conversation").
-define(ES_CONV_INDEX, "chat_conversation_v1").
-define(ES_CONV_TYPE, "conversation").

-define(ES_MSG_ALIAS, ?ES_CONV_ALIAS).
-define(ES_MSG_INDEX, ?ES_CONV_INDEX).
-define(ES_MSG_TYPE, "message").



%% ===================================================================
%% Users
%% ===================================================================

%% @private
read_user(SrvId, Id) ->
    read(SrvId, ?ES_USER_INDEX, ?ES_USER_TYPE, Id).


%% @private
write_user(SrvId, Id, Data) ->
    write(SrvId, ?ES_USER_INDEX, ?ES_USER_TYPE, Id, Data).


%% @private
delete_user(SrvId, Id) ->
    delete(SrvId, ?ES_USER_INDEX, ?ES_USER_TYPE, Id).


%% @private
delete_all_users(SrvId) ->
    delete_all(SrvId, ?ES_USER_INDEX, ?ES_USER_TYPE).


%% @private
list_users(SrvId, Opts) ->
    Opts2 = Opts#{
        sort_fields => #{
            <<"name">> => <<"name.keyword">>,
            <<"surname">> => <<"surname.keyword">>
        }
    },
    list(SrvId, ?ES_USER_ALIAS, ?ES_USER_TYPE, Opts2).



% ===================================================================
%% Conversations
%% ===================================================================

%% @private
read_conversation(SrvId, Id) ->
    read(SrvId, ?ES_CONV_INDEX, ?ES_CONV_TYPE, Id).


%% @private
write_conversation(SrvId, Id, Data) ->
    write(SrvId, ?ES_CONV_INDEX, ?ES_CONV_TYPE, Id, Data).


%% @private
delete_conversation(SrvId, Id) ->
    delete(SrvId, ?ES_CONV_INDEX, ?ES_CONV_TYPE, Id).

%% @private
delete_all_conversations(SrvId) ->
    delete_all(SrvId, ?ES_CONV_INDEX, ?ES_CONV_TYPE).


%% @private
list_conversations(SrvId, Opts) ->
    Opts2 = Opts#{
        sort_fields => #{
            <<"name">> => <<"name.keyword">>,
            <<"description">> => <<"description.keyword">>
        }
    },
    list(SrvId, ?ES_CONV_ALIAS, ?ES_CONV_TYPE, Opts2).



% ===================================================================
%% Messages
%% ===================================================================

%% @private
read_message(SrvId, Id) ->
    read(SrvId, ?ES_MSG_INDEX, ?ES_MSG_TYPE, Id).


%% @private
write_message(SrvId, Id, Data) ->
    write(SrvId, ?ES_MSG_INDEX, ?ES_MSG_TYPE, Id, Data).


%% @private
delete_message(SrvId, Id) ->
    delete(SrvId, ?ES_MSG_INDEX, ?ES_MSG_TYPE, Id).


%% @private
delete_all_messages(SrvId) ->
    delete_all(SrvId, ?ES_MSG_INDEX, ?ES_MSG_TYPE).


%% @private
list_messages(SrvId, Opts) ->
    list(SrvId, ?ES_MSG_ALIAS, ?ES_MSG_TYPE, Opts).




% ===================================================================
%% Internal - Indices
%% ===================================================================

create_indices(SrvId) ->
    create_user_index(SrvId),
    create_conv_index(SrvId),
    create_msg_index(SrvId).



%% @private
create_user_index(SrvId) ->
    case nkelastic_api:get_index(SrvId, ?ES_USER_INDEX) of
        {ok, _} ->
            user_mapping(SrvId),
            ok;
        {error, {es_error, <<"index_not_found_exception">>, _}} ->
            Opts = #{
                number_of_replicas => 2,
                analysis => nkelastic_api:spanish_ascii_analyzer()
            },
            case nkelastic_api:create_index(SrvId, ?ES_USER_INDEX, Opts) of
                ok ->
                    ?LLOG(warning, "index not found, created", []),
                    user_mapping(SrvId),
                    nkelastic_api:add_alias(SrvId, ?ES_USER_INDEX, ?ES_USER_ALIAS, #{}),
                    ok;
                {error, {es_error, Error, _}} ->
                    ?LLOG(error, "could not create index: ~p", [Error]),
                    {error, Error}
            end
    end.


%% @private
user_mapping(SrvId) ->
    Mapping = #{
        user_id => #{type => keyword},
        name => #{
            type => text,
            analyzer => spanish,
            fields => #{keyword => #{type=>keyword}}
        },
        surname => #{
            type => text,
            analyzer => spanish_ascii,
            fields => #{keyword=> #{type=>keyword}}
        },
        password => #{type => keyword},
        login => #{type => keyword},
        created_time => #{type => date}
    },
    nkelastic_api:add_mapping(SrvId, ?ES_USER_INDEX, ?ES_USER_TYPE, Mapping).



%% @private
create_conv_index(SrvId) ->
    case nkelastic_api:get_index(SrvId, ?ES_CONV_INDEX) of
        {ok, _} ->
            conv_mapping(SrvId),
            ok;
        {error, {es_error, <<"index_not_found_exception">>, _}} ->
            Opts = #{
                number_of_replicas => 2,
                analysis => nkelastic_api:spanish_ascii_analyzer()
            },
            case nkelastic_api:create_index(SrvId, ?ES_CONV_INDEX, Opts) of
                ok ->
                    ?LLOG(warning, "index not found, created", []),
                    conv_mapping(SrvId),
                    nkelastic_api:add_alias(SrvId, ?ES_CONV_INDEX, ?ES_CONV_ALIAS, #{}),
                    ok;
                {error, {es_error, Error, _}} ->
                    ?LLOG(error, "could not create index: ~p", [Error]),
                    {error, Error}
            end
    end.


%% @private
conv_mapping(SrvId) ->
    Mapping = #{
        conversation_id => #{type => keyword},
        name => #{
            type => text,
            analyzer => spanish,
            fields => #{keyword => #{type=>keyword}}
        },
        description => #{
            type => text,
            analyzer => spanish_ascii,
            fields => #{keyword=> #{type=>keyword}}
        },
        user_ids => #{type => keyword},
        created_time => #{type => date}
    },
    nkelastic_api:add_mapping(SrvId, ?ES_CONV_INDEX, ?ES_CONV_TYPE, Mapping).


%% @private
create_msg_index(SrvId) ->
    case nkelastic_api:get_index(SrvId, ?ES_CONV_INDEX) of
        {ok, _} ->
            msg_mapping(SrvId),
            ok;
        {error, {es_error, <<"index_not_found_exception">>, _}} ->
            Opts = #{
                number_of_replicas => 2,
                analysis => nkelastic_api:spanish_ascii_analyzer()
            },
            case nkelastic_api:create_index(SrvId, ?ES_CONV_INDEX, Opts) of
                ok ->
                    ?LLOG(warning, "index not found, created", []),
                    msg_mapping(SrvId),
                    nkelastic_api:add_alias(SrvId, ?ES_CONV_INDEX, ?ES_CONV_ALIAS, #{}),
                    ok;
                {error, {es_error, Error, _}} ->
                    ?LLOG(error, "could not create index: ~p", [Error]),
                    {error, Error}
            end
    end.


%% @private
msg_mapping(SrvId) ->
    Mapping = #{
        message_id => #{type => keyword},
        conversation_id => #{type => keyword},
        message => #{type => text},
        created_time => #{type => date},
        updated_time => #{type => date}
    },
    nkelastic_api:add_mapping(SrvId, ?ES_CONV_INDEX, ?ES_CONV_TYPE, Mapping).



% ===================================================================
%% Internal - Util
%% ===================================================================


remove_index(SrvId, Index) ->
    nkelastic_api:delete_index(SrvId, Index).


%% @private
read(SrvId, Index, Type, Id) ->
    nkelastic_api:get(SrvId, Index, Type, Id).


%% @private
write(SrvId, Index, Type, Id, Data) ->
    nkelastic_api:put(SrvId, Index, Type, Id, Data).


%% @private
delete(SrvId, Index, Type, Id) ->
    case nkelastic_api:delete(SrvId, Index, Type, Id) of
        {error, {<<"obj_not_found">>, _}} ->
            {error, obj_not_found};
        Other ->
            Other
    end.


%% @private
delete_all(SrvId, Index, Type) ->
    nkelastic_api:delete_all(SrvId, Index, Type).


%% @private
list(SrvId, Index, Type, Opts) ->
    nkelastic_api:list(SrvId, Index, Type, Opts).
