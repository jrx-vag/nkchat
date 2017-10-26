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

-ifndef(NKCHAT_HRL_).
-define(NKCHAT_HRL_, 1).

%% ===================================================================
%% Defines
%% ===================================================================

-define(CHAT_CONVERSATION, <<"conversation">>).
-define(CHAT_MESSAGE, <<"message">>).
-define(CHAT_SESSION, <<"chat.session">>).
-define(MEDIA_SESSION, <<"media.session">>).
-define(MEDIA_CALL, <<"media.call">>).

-define(CHAT_MSG_TYPE_ADDED_MEMBER, <<"chat.member_added">>).
-define(CHAT_MSG_TYPE_REMOVED_MEMBER, <<"chat.member_removed">>).


%% ===================================================================
%% Records
%% ===================================================================

-record(sdp_candidate, {
    mid = <<>>,
    index = <<>>,
    candidate = <<>>              %% <<>> means end
}).


-endif.

