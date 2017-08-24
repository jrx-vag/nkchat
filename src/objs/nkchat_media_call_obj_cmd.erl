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

%% @doc Conversation Object API
-module(nkchat_media_call_obj_cmd).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([cmd/2]).

-include("nkchat.hrl").
-include_lib("nkdomain/include/nkdomain.hrl").
-include_lib("nkservice/include/nkservice.hrl").


%% ===================================================================
%% API
%% ===================================================================

%%cmd(<<"add_member">>, #nkreq{data=#{id:=ConvId, member_id:=MemberId}, srv_id=SrvId}) ->
%%    case nkchat_media_call_obj:add_member(SrvId, ConvId, MemberId) of
%%        {ok, MemberObjId} ->
%%            {ok, #{<<"member_id">>=>MemberObjId}};
%%        {error, Error} ->
%%            {error, Error}
%%    end;
%%
%%cmd(<<"remove_member">>, #nkreq{data=#{id:=ConvId, member_id:=MemberId}, srv_id=SrvId}) ->
%%    case nkchat_media_call_obj:remove_member(SrvId, ConvId, MemberId) of
%%        ok ->
%%            {ok, #{}};
%%        {error, Error} ->
%%            {error, Error}
%%    end;

cmd(<<"find_member_calls">>, #nkreq{data=Data, srv_id=SrvId}=Req) ->
    case nkdomain_api_util:get_id(?DOMAIN_DOMAIN, domain_id, Data, Req) of
        {ok, DomainId} ->
            case nkdomain_api_util:get_id(?DOMAIN_USER, member_id, Data, Req) of
                {ok, MemberId} ->
                    case nkchat_media_call_obj:find_member_calls(SrvId, DomainId, MemberId) of
                        {ok, List} ->
                            List2 = [#{<<"call_id">>=>ConvId, <<"type">>=>Type} || {ConvId, Type} <-List],
                            {ok, #{<<"data">>=>List2}};
                        {error, Error} ->
                            {error, Error}
                    end;
                _ ->
                    {error, user_unknown}
            end;
        {error, Error} ->
            {error, Error}
    end;

cmd(<<"find_calls_with_members">>, #nkreq{data=Data, srv_id=SrvId}=Req) ->
    case nkdomain_api_util:get_id(?DOMAIN_DOMAIN, domain_id, Data, Req) of
        {ok, DomainId} ->
            #{member_ids:=MemberIds} = Data,
            case nkchat_media_call_obj:find_calls_with_members(SrvId, DomainId, MemberIds) of
                {ok, List} ->
                    List2 = [#{<<"conversation_id">>=>ConvId, <<"type">>=>Type} || {ConvId, Type} <-List],
                    {ok, #{<<"data">>=>List2}};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end;

cmd(<<"send_candidate">>, #nkreq{data=Data, srv_id=SrvId, user_id=MemberId}) ->
    #{id:=CallId, sdp_mid:=MId, sdp_line_index:=Index, sdp_candidate:=Line} = Data,
    Candidate = #sdp_candidate{mid=MId, index=Index, candidate=Line},
    nkchat_media_call_obj:send_candidate(SrvId, CallId, MemberId, Candidate);

cmd(<<"send_candidate_end">>, #nkreq{data=Data, srv_id=SrvId, user_id=MemberId}) ->
    #{id:=CallId} = Data,
    nkchat_media_call_obj:send_candidate(SrvId, CallId, MemberId, #sdp_candidate{});


cmd(<<"set_status">>, #nkreq{data=#{id:=CallId}=Data, srv_id=SrvId, user_id=MemberId}) ->
    Opts = maps:with([audio, video], Data),
    nkchat_media_call_obj:set_status(SrvId, CallId, MemberId, Opts);

cmd(Cmd, Req) ->
    lager:error("NKLOG CALL API ~p", [Cmd]),
    nkdomain_obj_api:api(Cmd, ?CHAT_CONVERSATION, Req).

