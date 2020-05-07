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
cmd(<<"add_log">>, #nkreq{data=#{call_id:=CallId}=Data}=Req) ->
    case nkdomain_api_util:get_id(?MEDIA_SESSION, Data, Req) of
        {ok, Id} ->
            Type = maps:get(type, Data, <<>>),
            LogData = maps:get(data, Data, #{}),
            nkchat_media_call_obj:add_log(CallId, Id, Type, LogData);
        {error, Error} ->
            {error, Error}
    end;

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

cmd(<<"get_call_info">>, #nkreq{data=#{call_id:=CallId}}) ->
    nkchat_media_call_obj:get_info(CallId);

cmd(<<"invite">>, #nkreq{data=#{user_id:=UserId}=Data}=Req) ->
    case nkdomain_api_util:get_id(?MEDIA_SESSION, Data, Req) of
        {ok, Id} ->
            Opts = maps:with([call_name, sdp, trickle_ice, ttl, audio, video, screen, conversation_id], Data),
            case nkchat_media_session_obj:invite(Id, UserId, Opts) of
                {ok, InviteId} ->
                    {ok, #{<<"invite_id">> => InviteId}};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end;

cmd(<<"cancel_invite">>, #nkreq{data=#{invite_id:=InviteId}=Data}=Req) ->
    case nkdomain_api_util:get_id(?MEDIA_SESSION, Data, Req) of
        {ok, _Id} ->
            nkchat_media_session_obj:cancel_invite(InviteId);
        {error, Error} ->
            {error, Error}
    end;


cmd(<<"accept_invite">>, #nkreq{data=#{invite_id:=InviteId}=Data}=Req) ->
    case nkdomain_api_util:get_id(?MEDIA_SESSION, Data, Req) of
        {ok, Id} ->
            Opts = maps:with([sdp, trickle_ice, audio, video, screen], Data),
            case nkchat_media_session_obj:accept_invite(Id, InviteId, Opts) of
                ok ->
                    {ok, #{}};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end;

cmd(<<"reject_invite">>, #nkreq{data=#{invite_id:=InviteId}=Data}=Req) ->
    case nkdomain_api_util:get_id(?MEDIA_SESSION, Data, Req) of
        {ok, _Id} ->
            nkchat_media_session_obj:reject_invite(InviteId);
        {error, Error} ->
            {error, Error}
    end;

cmd(<<"send_candidate">>, #nkreq{data=Data}=Req) ->
    case nkdomain_api_util:get_id(?MEDIA_SESSION, Data, Req) of
        {ok, Id} ->
            #{call_id:=CallId, sdp_mid:=MId, sdp_line_index:=Index, candidate:=Line} = Data,
            Candidate = #sdp_candidate{mid=MId, index=Index, candidate=Line},
            nkchat_media_call_obj:send_candidate(CallId, Id, Candidate);
        {error, Error} ->
            {error, Error}
    end;

cmd(<<"send_candidate_end">>, #nkreq{data=Data}=Req) ->
    case nkdomain_api_util:get_id(?MEDIA_SESSION, Data, Req) of
        {ok, Id} ->
            #{call_id:=CallId} = Data,
            nkchat_media_call_obj:send_candidate(CallId, Id, #sdp_candidate{});
        {error, Error} ->
            {error, Error}
    end;

cmd(<<"set_has_connected">>, #nkreq{data=#{call_id:=CallId}=Data}=Req) ->
    case nkdomain_api_util:get_id(?MEDIA_SESSION, Data, Req) of
        {ok, Id} ->
            HasConnected = maps:get(has_connected, Data, false),
            nkchat_media_call_obj:set_has_connected(CallId, Id, HasConnected);
        {error, Error} ->
            {error, Error}
    end;

cmd(<<"set_status">>, #nkreq{data=#{call_id:=CallId}=Data}=Req) ->
    case nkdomain_api_util:get_id(?MEDIA_SESSION, Data, Req) of
        {ok, Id} ->
            Opts = maps:with([audio, video, screen], Data),
            nkchat_media_call_obj:set_status(CallId, Id, Opts);
        {error, Error} ->
            {error, Error}
    end;

cmd(<<"hangup_call">>, #nkreq{data=#{call_id:=CallId}}) ->
    nkchat_media_call_obj:hangup(CallId, user_hangup);

cmd(Cmd, Req) ->
    nkdomain_obj_cmd:cmd(Cmd, ?MEDIA_SESSION, Req).



%% ===================================================================
%% Internal
%% ===================================================================
