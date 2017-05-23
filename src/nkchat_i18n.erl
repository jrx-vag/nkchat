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


i18n() -> #{
    en => #{
        domain_tree_resources_chat_conversations => "Conversations",
        domain_tree_resources_chat_messages => "Chat Messages",

        domain_tree_sessions_chat_sessions => "Chat"
    },
    es => #{
        domain_tree_resources_chat_conversations => "Conversaciones",
        domain_tree_resources_chat_messages => "Mensajes de chat",

        domain_tree_sessions_chat_sessions => "Chat"
    }
}.


reload() ->
    ok = nklib_i18n:load(?MODULE).