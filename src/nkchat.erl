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


-module(nkchat).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-export_type([search_type/0]).


%% ===================================================================
%% Types
%% ===================================================================

-type search_type() ::
    {member_conversations, Domain::nkdomain:id(), Member::nkdomain:id()} |
    {conversations_with_members, Domain::nkdomain:id(), MemberIds::[nkdomain:obj_id()]} |
    {conversations_messages, Conv::nkdomain:id(),
        #{from => integer, size => integer, start_date => integer, end_date => integer, inclusive => boolean}}.



%% ===================================================================
%% Public functions
%% ===================================================================
