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

%% @doc Session Object API
-module(nkchat_session_obj_api).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([cmd/4]).

-include("nkchat.hrl").

%% ===================================================================
%% API
%% ===================================================================

%% @doc
cmd('', create, Data, State) ->
    #{obj_name:=Name, description:=Desc} = Data,
    #{srv_id:=SrvId, domain:=Domain} = State,
    case nkchat_session_obj:create(SrvId, Domain, Name, Desc) of
        {ok, ObjId, Path, _Pid} ->
            State2 = nkdomain_api_util:add_id(?CHAT_CONVERSATION, ObjId, State),
            {ok, #{obj_id=>ObjId, path=>Path}, State2};
        {error, Error} ->
            {error, Error, State}
    end;


cmd('', Cmd, Data, State) ->
    nkdomain_api_util:cmd_common(?CHAT_SESSION, Cmd, Data, State);

cmd(_Sub, _Cmd, _Data, State) ->
    {error, not_implemented, State}.
