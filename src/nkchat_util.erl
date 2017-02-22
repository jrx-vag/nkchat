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

-module(nkchat_util).

-export([http/4, http_raw/4]).



%% ===================================================================
%% Public
%% ===================================================================


%% @private
http(Method, Url, Hds, Body) ->
    case http_raw(Method, Url, Hds, Body) of - 
        {ok, Code, RespHds, RespBody} ->
            RespBody2 = case nklib_util:get_value(<<"Content-Type">>, RespHds) of
                <<"application/json">> ->
                    nklib_json:decode(RespBody);
                _ ->
                    RespBody
            end,
            {ok, Code, RespHds, RespBody2};
        {error, Error} ->
            {error, Error}
    end.


%% @private
http_raw(Method, Url, Hds, Body) ->
    Body2 = case is_map(Body) orelse is_list(Body) of
        true -> nklib_json:encode(Body);
        false -> Body
    end,
    Ciphers = ssl:cipher_suites(),
    % Hackney fails with its default set of ciphers
    % See hackney.ssl#44
    HttpOpts = [
        {connect_timeout, 15000},
        {recv_timeout, 15000},
        insecure,
        with_body,
        {ssl_options, [{ciphers, Ciphers}]}
    ],
    hackney:request(Method, Url, Hds, Body2, HttpOpts).