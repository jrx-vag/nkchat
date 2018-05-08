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

-module(nkchat_i18n).
-behavior(nklib_i18n).

-export([i18n/0, reload/0]).

-include_lib("nkdomain/include/nkdomain.hrl").

i18n() -> #{
    en => #{
        "domain_tree_resources__conversation" => "Conversations",
        "domain_tree_resources__message" => "Chat Messages",

        "domain_tree_sessions__chat.session" => "Chat",

        "domain_tree_sessions__media.session" => "Media",
        "domain_tree_sessions__media.call" => "Calls",

        "domain_column_conversation" => "CONVERSATION",
        "domain_column_duration" => "DURATION",
        "domain_column_file_id" => "FILE",
        "domain_column_members" => "MEMBERS",
        "domain_column_text" => "MESSAGE",
        "domain_column_type" => "TYPE"


    },
    es => #{
        "domain_tree_resources__conversation" => "Conversaciones",
        "domain_tree_resources__message" => "Mensajes de chat",

        "domain_tree_sessions__chat.session" => "Chat",

        "domain_tree_sessions__media.session" => "Media",
        "domain_tree_sessions__media.call" => "Llamadas",

        "domain_column_conversation" => <<"CONVERSACIÓN"/utf8>>,
        "domain_column_duration" => <<"DURACIÓN"/utf8>>,
        "domain_column_file_id" => "FICHERO",
        "domain_column_members" => "MIEMBROS",
        "domain_column_text" => "MENSAJE",
        "domain_column_type" => "TIPO"
    }
}.


reload() ->
    ok = nklib_i18n:load(?NKROOT, ?MODULE).