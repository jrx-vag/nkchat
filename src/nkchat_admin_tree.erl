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

%% @doc
-module(nkchat_admin_tree).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-export([element_action/5]).



%% ===================================================================
%% Public
%% ===================================================================

%% @doc
%element_action(<<"domain_tree_resources_message">>=Key, selected, Value, Updates, Session) ->
%    #{domain_id:=DomainId} = Session,
%    Table = nkchat_message_obj_ui:table(root, DomainId),
%    Detail = #{
%        id => <<"domain_detail_chat_messages_table">>,
%        class => webix_ui,
%        value => Table
%    },
%    {Updates2, Session2} = nkadmin_util:update_detail(<<"messages">>, Detail, Updates, Session),
%    {continue, [Key, selected, Value, Updates2, Session2]};

element_action(_Id, _Action, _Value, _Updates, _Session) ->
    continue.



