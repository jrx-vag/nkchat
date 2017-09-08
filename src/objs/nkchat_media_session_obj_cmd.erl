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
-module(nkchat_media_session_obj_cmd).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([cmd/2]).

-include("nkchat.hrl").
-include_lib("nkdomain/include/nkdomain.hrl").
-include_lib("nkevent/include/nkevent.hrl").
-include_lib("nkservice/include/nkservice.hrl").


-define(SESSION_DEF_EVENT_TYPES, [
    <<"unloaded">>,
    <<"incoming_call">>,
    <<"call_hangup">>
]).


%% ===================================================================
%% API
%% ===================================================================


%% @doc
cmd(<<"start">>, #nkreq{session_module=nkapi_server}=Req) ->
    #nkreq{data=Data, session_pid=Pid, user_id=UserId} = Req,
    case nkdomain_api_util:get_id(?DOMAIN_DOMAIN, domain_id, Data, Req) of
        {ok, DomainId} ->
            Opts = #{
                session_events => maps:get(session_events, Data, []),
                session_link => {nkapi_server, Pid}
            },
            case nkchat_media_session_obj:start( DomainId, UserId, Opts) of
                {ok, SessId, _Pid} ->
                    Req2 = nkdomain_api_util:add_id(?MEDIA_SESSION, SessId, Req),
                    {ok, #{obj_id=>SessId}, Req2};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end;

cmd(<<"stop">>, #nkreq{data=Data, user_state=_UserState}=Req) ->
    case nkdomain_api_util:get_id(?MEDIA_SESSION, Data, Req) of
        {ok, SessId} ->
            case nkdomain:unload(SessId, user_stop) of
                ok ->
                    {ok, #{}};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end;

cmd(<<"launch_notifications">>, #nkreq{data=Data}=Req) ->
    case nkdomain_api_util:get_id(?MEDIA_SESSION, Data, Req) of
        {ok, Id} ->
            nkchat_media_session_obj:launch_notifications(Id);
        {error, Error} ->
            {error, Error}
    end;

cmd(<<"get_calls">>, #nkreq{data=Data}=Req) ->
    case nkdomain_api_util:get_id(?MEDIA_SESSION, Data, Req) of
        {ok, Id} ->
            case nkchat_media_session_obj:get_calls(Id) of
                {ok, List} ->
                    {ok, #{call_ids=>List}};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end;

cmd(<<"get_call_info">>, #nkreq{data=#{conversation_id:=ConvId}=Data}=Req) ->
    case nkdomain_api_util:get_id(?MEDIA_SESSION, Data, Req) of
        {ok, Id} ->
            case nkchat_media_session_obj:get_call_info(Id, ConvId) of
                {ok, Data2} ->
                    {ok, Data2};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end;


cmd(<<"invite">>, #nkreq{data=#{user_id:=UserId}=Data}=Req) ->
    case nkdomain_api_util:get_id(?MEDIA_SESSION, Data, Req) of
        {ok, Id} ->
            Opts = maps:with([call_name, sdp, trickle_ice, ttl, audio, video], Data),
            case nkchat_media_session_obj:invite(Id, UserId, Opts) of
                {ok, TokenId} ->
                    {ok, #{<<"invite_id">> => TokenId}};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end;

cmd(<<"cancel_invite">>, #nkreq{data=#{invite_id:=InvId}=Data}=Req) ->
    case nkdomain_api_util:get_id(?MEDIA_SESSION, Data, Req) of
        {ok, Id} ->
            nkchat_media_session_obj:cancel_invite(Id, InvId);
        {error, Error} ->
            {error, Error}
    end;


cmd(<<"accept_invite">>, #nkreq{data=#{invite_id:=InviteId}=Data}=Req) ->
    case nkdomain_api_util:get_id(?MEDIA_SESSION, Data, Req) of
        {ok, Id} ->
            Opts = maps:with([sdp, trickle_ice, audio, video], Data),
            case nkchat_media_session_obj:accept_invite(Id, InviteId, Opts) of
                {ok, CallId} ->
                    {ok, #{call_id=>CallId}};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end;

cmd(<<"reject_invite">>, #nkreq{data=#{invite_id:=InviteId}=Data}=Req) ->
    case nkdomain_api_util:get_id(?MEDIA_SESSION, Data, Req) of
        {ok, Id} ->
            nkchat_media_session_obj:reject_invite(Id, InviteId);
        {error, Error} ->
            {error, Error}
    end;

cmd(<<"hangup_call">>, #nkreq{data=#{call_id:=CallId}=Data}=Req) ->
    case nkdomain_api_util:get_id(?MEDIA_SESSION, Data, Req) of
        {ok, Id} ->
            nkchat_media_session_obj:call_hangup(Id, CallId);
        {error, Error} ->
            {error, Error}
    end;

cmd(Cmd, Req) ->
    nkdomain_obj_cmd:api(Cmd, ?MEDIA_SESSION, Req).



%% ===================================================================
%% Internal
%% ===================================================================
