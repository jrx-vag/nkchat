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

-module(nkchat_mm_proxy_web).
-export([init/2, terminate/3]).

-define(HOST, <<"https://mde.netc.io:9443">>).




%% ===================================================================
%% Users
%% ===================================================================


%% @doc
pre_users(_Other, State) ->
    {proxy, State}.


%% @doc
post_users([<<"initial_load">>], #{resp_body:=Body}=State) ->
    Data1 = nklib_json:decode(Body),
    #{<<"client_cfg">> := ClientCfg1} = Data1,
    ClientCfg2 = ClientCfg1#{<<"SiteURL">> := <<"http://127.0.0.1:9443">>},
    Data2 = Data1#{<<"client_cfg">>:=ClientCfg2},
    lager:info("Initial load: \n~s", [nklib_json:encode_pretty(Data2)]),
    {proxy, Data2, State};

post_users([<<"login">>], #{resp_hds:=Hds, resp_body:=Body}=State) ->
    Token = nklib_util:get_value(<<"Token">>, Hds),
    _Cookie = nklib_util:get_value(<<"Set-Cookie">>, Hds),
    _ReqId = nklib_util:get_value(<<"X-Request-Id">>, Hds),
    _VsnId = nklib_util:get_value(<<"X-Version-Id">>, Hds),
    Data = nklib_json:decode(Body),
    lager:info("User data: ~s", [nklib_json:encode_pretty(Data)]),
    lager:notice("Token: ~s", [Token]),
    nklib_store:put({?MODULE, token}, Token),
    {proxy, State};

post_users([<<"logout">>], State) ->
    nklib_store:del({?MODULE, token}),
    {proxy, State};

post_users(Other, State) ->
    lager:error("Users: ~p", [Other]),
    {proxy, State}.



%% ===================================================================
%% Emoji
%% ===================================================================

%% @doc
pre_emoji([<<"list">>], #{req_hds:=Hds}=State) ->
    lager:error("EM: ~p", [Hds]),
    {proxy, State};

pre_emoji(_Other, State) ->
    {proxy, State}.



%% ===================================================================
%% Cowboy callbacks
%% ===================================================================

%% @private
init(Req, Opts) ->
    Method = case cowboy_req:method(Req) of
        <<"PUT">> -> put;
        <<"GET">> -> get;
        <<"POST">> -> post;
        _ -> unknown
    end,
    Path = cowboy_req:path_info(Req),
    Url = nklib_util:bjoin([?HOST|Path], <<"/">>),
    Hds = cowboy_req:headers(Req),
    case Method of
        get -> 
            Body = <<>>;
        post ->
            {ok, Body, _} = cowboy_req:body(Req)
    end,
    State1 = #{
        method => method,
        path => Path,
        req_hds => Hds,
        req_body => Body
    },
    Req2 = case pre_process(Path, State1) of
        {proxy, State2} ->
            case http(Method, Url, Hds, Body) of
                {ok, Code,  Hds2, Body2} ->
                    State3 = State2#{
                        code => Code,
                        resp_hds => Hds2,
                        resp_body => Body2
                    },
                    case post_process(Path, State3) of
                        {proxy, _State4} ->
                            reply(Code, Hds2, Body2, Req);
                        {proxy, Body3, _State4} ->
                            reply(Code, Hds2, Body3, Req);
                        error ->
                            reply(500, [], <<>>, Req)
                    end;
                error ->
                    reply(500, [], <<>>, Req)
            end;
        error ->
            reply(500, [], <<>>, Req)
    end,
    {ok, Req2, Opts}.


%% @private
terminate(_Reason, _Req, _Opts) ->
    ok.


%% ===================================================================
%% Internal
%% ===================================================================


%% @private
http(Method, Url, Hds, Body) ->
    Body2 = case is_map(Body) of
        true -> nklib_json:encode(Body);
        false -> Body
    end,
    Hds2 = case nklib_store:get({?MODULE, token}) of
        not_found ->
            lager:warning("NOT Added token for ~s", [Url]),
            Hds;
        Token -> 
            lager:warning("Added token for ~s", [Url]),
            [{<<"Authorization">>, <<"Bearer ", Token/binary>>}|Hds]
    end,
    Ciphers = ssl:cipher_suites(),
    % Hackney fails with its default set of ciphers
    % See hackney.ssl#44
    HttpOpts = [
        {connect_timeout, 5000},
        {recv_timeout, 5000},
        insecure,
        with_body,
        {ssl_options, [{ciphers, Ciphers}]}
    ],
    Start = nklib_util:l_timestamp(),
    case hackney:request(Method, Url, Hds2, Body2, HttpOpts) of
        {ok, Code, RHds, RBody} ->
            Time = (nklib_util:l_timestamp() - Start) div 1000,
            CT = nklib_util:get_value(<<"Content-Type">>, RHds),
            case Code of
                200 ->
                    lager:info("~p ~s: ~s (~p msecs)", [Method, Url, CT, Time]);
                _ ->
                    lager:warning("~p ~s: ~p", [Method, Url, Code])
            end,
            RHds2 = lists:keydelete(<<"Content-Length">>, 1, RHds),
            {ok, Code, RHds2, RBody};
        {error, Error} ->
            lager:error("Proxy Error: ~s (~p) ~p", [Url, Method, Error]),
            error
    end.



%wss://mde.netc.io:9443/api/v3/users/websocket

%% @private
pre_process([<<"api">>, <<"v3">>, <<"users">>|Rest], State) ->
    pre_users(Rest, State);

pre_process([<<"api">>, <<"v3">>, <<"emoji">>|Rest], State) ->
    pre_emoji(Rest, State);

pre_process(_Path, State) ->
    {proxy, State}.


%% @private
post_process([<<"api">>, <<"v3">>, <<"users">>|Rest], #{code:=200}=State) ->
    post_users(Rest, State);

post_process(_Method, State) ->
    {proxy, State}.


%% @private
reply(Code, Hds, Body, Req) ->
    Body2 = case is_map(Body) of
        true -> nklib_json:encode_pretty(Body);
        false -> Body
    end,
    cowboy_req:reply(Code, Hds, Body2, Req).




