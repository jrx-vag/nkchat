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

-module(nkchat_mm_proxy_server_http).
-export([init/2, terminate/3]).
-export_type([reply/0, method/0, code/0, path/0, req/0, http_qs/0]).

-define(HOST, "chat.dkv.netc.io:8065").

% To debug, set debug => [nkchat_mm_proxy_server_http]

-define(DEBUG(Txt, Args, State),
    case erlang:get(nkchat_mm_proxy_server_http_debug) of
        true -> ?LLOG(debug, Txt, Args, State);
        _ -> ok
    end).

-define(LLOG(Type, Txt, Args, State),
    lager:Type("NkCHAT MM Proxy HTTP "++Txt, 
               Args)).

% -define(MSG(Txt, Args, State),
%     case erlang:get(nkchat_mm_proxy_server_http_debug) of
%         true -> print(Txt, Args, State);
%         _ -> ok
%     end).





%% ===================================================================
%% Types
%% ===================================================================


-record(state, {
    srv_id :: nkservice:id(),
    remote :: binary(),
    user :: binary(),
    user_state :: map(),
    req :: term(),
    method :: binary(),
    path :: [binary()],
    ct :: binary()
}).

-type method() :: get | post | head | delete | put.

-type code() :: 100 .. 599.

-type header() :: [{binary(), binary()}].

-type body() ::  Body::binary()|map().

-type state() :: map().

-type reply() ::
    {http, code(), [header()], body(), state()} |
    {proxy, state()}.

-type http_qs() ::
    [{binary(), binary()|true}].

-type req() :: #state{}.

-type path() :: [binary()].



%% ===================================================================
%% Public
%% ===================================================================

%% @doc
-spec get_body(req(), #{max_size=>integer(), parse=>boolean()}) ->
    binary() | map().

get_body(#state{ct=CT, req=Req}, Opts) ->
    MaxBody = maps:get(max_size, Opts, 100000),
    case cowboy_req:body_length(Req) of
        BL when is_integer(BL), BL =< MaxBody ->
            {ok, Body, _} = cowboy_req:body(Req),

            case maps:get(parse, Opts, false) of
                true ->
                    case CT of
                        <<"application/json">> when byte_size(Body)>0 ->
                            case nklib_json:decode(Body) of
                                error ->
                                    throw({400, [], <<"Invalid json">>});
                                Json ->
                                    Json
                            end;
                        _ ->
                            Body
                    end;
                _ ->
                    Body
            end;
        _ ->
            throw({400, [], <<"Body too large">>})
    end.



%% @doc
-spec get_headers(req()) ->
    [{binary(), binary()}].

get_headers(#state{req=Req}) ->
    cowboy_req:headers(Req).
    

%% ===================================================================
%% Cowboy callbacks
%% ===================================================================

%% @private
init(Req, [{srv_id, SrvId}]) ->
    {Ip, Port} = cowboy_req:peer(Req),
    Remote = <<
        (nklib_util:to_host(Ip))/binary, ":",
        (nklib_util:to_binary(Port))/binary
    >>,
    Method = case cowboy_req:method(Req) of
        <<"PUT">> -> put;
        <<"GET">> -> get;
        <<"POST">> -> post
    end,
    Url = cowboy_req:url(Req),
    Path1 = case cowboy_req:path_info(Req) of
        [<<>>|Rest] -> Rest;
        Rest -> Rest
    end,
    Path2 = case binary:at(Url, byte_size(Url)-1) of
        $/ -> Path1 ++ [<<>>];
        _ -> Path1
    end,
    UserState = #{srv_id=>SrvId, remote=>Remote},
    State1 = #state{
        srv_id = SrvId, 
        remote = Remote,
        user = <<>>,
        user_state = UserState,
        req = Req,
        method = Method,
        path = Path2,
        ct = cowboy_req:header(<<"content-type">>, Req)
    },
    set_log(State1),
    ?DEBUG("received ~p (~p) from ~s", [Method, Url, Remote], State1),
    try
        Reply = handle(nkchat_mm_proxy_http, [Method, Path2, State1], State1),
        process(Reply)
    catch
        throw:{Code, Hds, Body} ->
            send_http_reply(Code, Hds, Body, State1)
    end.


%% @private
terminate(_Reason, _Req, _Opts) ->
    ok.


%% ===================================================================
%% Process
%% ===================================================================

%% @private
process({http_ok, State}) ->
    send_http_reply(200, [], <<>>, State);

% process({http_error, Error, State}) ->
%     send_http_error(Error, State);

process({http, Code, Hds, Body, State}) ->
    send_http_reply(Code, Hds, Body, State);

process({proxy, #state{path=[<<>>]}=State}) ->
    send_static_proxy(State);

process({proxy, #state{path=[<<"static">>|_]}=State}) ->
    send_static_proxy(State);

process({proxy, #state{path=[<<"select_team">>]}=State}) ->
    send_static_proxy(State);

process({proxy, #state{path=[<<"api">>, <<"v3">>|Rest], method=Method}=State}) ->
    Body = case Method of
        post ->
            get_body(State, #{parse=>true});
        _ ->
            <<>>
    end,
    ?LLOG(info, "processing API ~p ~p", [Method, Rest], State),
    process_api(Rest, Body, State);


process({proxy, #state{path=Path}=State}) ->
    lager:warning("Unexpected path: ~p", [Path]),
    send_static_proxy(State).



%% ===================================================================
%% Process API
%% ===================================================================


%% @private
%% Body is be already decoded
process_api([<<"users">>, <<"initial_load">>], Body, State) ->
    case do_proxy(Body, State) of
        {ok, 200, RespHds, RespBody} ->
            #{<<"client_cfg">>:=Cfg1} = RespBody,
            Cfg2 = Cfg1#{<<"SiteURL">>=><<"https://chat.dkv.netc.io">>},
            RespBody2 = RespBody#{<<"client_cfg">>:=Cfg2},
            lager:info("Updated inital_load"),
            send_http_reply(200, RespHds, RespBody2, State);
        error ->
            send_http_error(State)
    end;


%% First login comes with 
%% Cookie: rocketchatscreenshare=chrome
%% "token" is json is empty
%%
%% Server replies with headers:
%% - Set-Cookie: MMAUTHTOKEN=9fui...;n Path...
%% - Token: 9fui...
%% - X-Request-Id: uqds...
%% - X-Version-Id: 3.5.0...
%%
%% In the json:
%% - id: Id of the user?
%%
%% Client sends another initial_load con
%% Cookie: rocketchatscreenshare=chrome; MMAUTHTOKEN=9fui...
%%
%% Server replies with more data (another X-Request-Id)


process_api([<<"users">>, <<"login">>], #{<<"login_id">>:=User}=Body, State) ->
    case do_proxy(Body, State) of
        {ok, 200, RespHds, RespBody} ->
            send_http_reply(200, RespHds, RespBody, State#state{user=User});
        error ->
            send_http_error(State)
    end;

process_api(_Path, Body, State) ->
    case do_proxy(Body, State) of
        {ok, Code, RespHds, RespBody} ->
            send_http_reply(Code, RespHds, RespBody, State);
        error ->
            send_http_error(State)
    end.



%% ===================================================================
%% Internal
%% ===================================================================


%% @private
do_proxy(Body, #state{method=Method}=State) ->
    Url = make_proxy_url(State),
    Hds = get_headers(State),
    ?DEBUG("to: ~s", [Url], State),
    ?DEBUG("orig headers: ~p", [Hds], State),
    case is_map(Body) orelse is_list(Body) of
        true ->
            ?DEBUG("~p ~s request: ~s", 
                  [Method, Url, nklib_json:encode_pretty(Body)], State);
        false ->
            ok
    end,
    case nkchat_util:http(Method, Url, Hds, Body) of
        {ok, Code, RespHds, RespBody} ->
            case is_map(RespBody) orelse is_list(RespBody) of
                true ->
                    ?DEBUG("~p ~s response (~p): ~s", 
                          [Method, Url, Code, nklib_json:encode_pretty(RespBody)], State);
                false ->
                    ok
            end,
            {ok, Code, RespHds, RespBody};
        {error, Error} ->
            ?LLOG(warning, "~p ~s error: ", [Method, Url, Error], State),
            error
    end.



%% @private
send_static_proxy(#state{method=Method}=State) ->
    Url = make_proxy_url(State),
    Hds = get_headers(State),
    Body = get_body(State, #{}),
    ?DEBUG("static proxy to: ~s", [Url], State),
    case nkchat_util:http_raw(Method, Url, Hds, Body) of
        {ok, Code, RespHds, RespBody} ->
            send_http_reply(Code, RespHds, RespBody, State);
        {error, Error} ->
            ?LLOG(warning, "static proxy error: ~p", [Error], State),
            send_http_reply(500, [], <<>>, State)
    end.


%% @private
make_proxy_url(#state{path=Path, req=Req}) ->
    Url = nklib_util:bjoin([<<"http://", ?HOST>>|Path], <<"/">>),
    case cowboy_req:qs(Req) of
        <<>> -> Url;
        Qs -> <<Url/binary, $?, Qs/binary>>
    end.




%% @private
set_log(#state{srv_id=SrvId}=State) ->
    Debug = case nkservice_util:get_debug_info(SrvId, ?MODULE) of
        {true, _} -> true;
        _ -> false
    end,
    % lager:error("HTTP DEBUG: ~p", [Debug]),
    put(nkchat_mm_proxy_server_http_debug, Debug),
    State.


%% @private
send_http_reply(Code, Hds, Body, #state{req=Req}) ->
    Hds2 = filter_hds(Hds),
    Body2  = case is_map(Body) orelse is_list(Body) of
        true -> 
            nklib_json:encode(Body);
        false ->
            nklib_util:to_binary(Body)
    end,
    {ok, cowboy_req:reply(Code, Hds2, Body2, Req), []}.


%% @private
filter_hds(Hds) ->
    lists:filter(
        fun({K, _}) ->
            case K of
                <<"Content-Type">> -> true;
                <<"X-Request-Id">> -> true;
                <<"X-Version-Id">> -> true;
                <<"Token">> -> true;
                <<"Set-Cookie">> -> true;
                <<"ETag">> -> true;
                <<"Expires">> -> true;
                _ -> false
            end
        end,
        Hds).


%% @private
send_http_error(State) ->
    send_http_reply(500, [], <<>>, State).






%% @private
handle(Fun, Args, State) ->
    nklib_gen_server:handle_any(Fun, Args, State, #state.srv_id, #state.user_state).


