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

-module(nkchat_mm_client).
-compile([export_all]).

-define(HOST, <<"http://chat.dkv.netc.io:8065">>).


%% ===================================================================
%% Types
%% ===================================================================

-type user_id() :: binary().

-type team_id() :: binary().

-type channel_id() :: binary().

-type token() :: binary().



-type error() :: 
    {error, nkservice:error()} |
    {error, {mm_error, binary(), binary()}} |
    {error, {Code::integer(), map()}}.




%% ===================================================================
%% Users
%% ===================================================================

%% @private
login() ->
    login("carlos", "carlos12").


%% @private
login(User, Pass) ->
    login(User, Pass, <<>>).


%% @private
login(User, Pass, Device) ->
    Data = #{
        login_id => bin(User),
        password => bin(Pass),
        device => bin(Device)
    },
    case post_raw("/users/login", Data, <<>>) of
        {ok, #{token:=Token, <<"id">>:=Id}=Resp} ->
            {ok, Id, Token, Resp};
        {error, {mm_error, <<"store.sql_user.get_for_login.app_error">>, _}} ->
            {error, user_not_foubd};
        {error, {mm_error, <<"api.user.check_user_password.invalid.app_error">>, _}} ->
            {error, invalid_password};
        {error, Error} ->
            {error, Error}
    end.



%% @private
logout(T) ->
    post("/users/logout", <<>>, T).


%% @private      
user_get(T) ->
    get("/users/me", T).


%% @private
user_set_pass(UserId, OldPass, NewPass, T) ->
    Data = #{
        user_id => bin(UserId),
        current_password => bin(OldPass),
        new_password => bin(NewPass)
    },
    case post("/users/newpassword", Data, T) of
        {error, {mm_error, <<"api.user.update_password.incorrect.app_error">>, _}} ->
            {error, invalid_password};
        Other ->
            Other
    end.


-type user_create() ::
    #{
        email => binary(),          % mandatory
        username => binary(),       % mandatory
        password => binary(),       % mandatory
        first_name => binary(),
        last_name => binary(),
        nickname => binary(),
        locale => binary(),
        props => map()
    }.


%% @doc
-spec user_create(user_create(), token()) ->
    ok | {error, term()}.

user_create(Data, T) ->
    post("/users/create", Data, T).




%% @doc
-spec user_list(token()) ->
    {ok, [map()]} | {error, term()}.

user_list(T) ->
    user_list(0, 1000, T).


%% @doc
-spec user_list(integer(), integer(), token()) ->
    {ok, [map()]} | {error, term()}.

user_list(Offset, Limit, T) ->
    get(["/users/", bin(Offset), "/", bin(Limit)], T).


%% @doc
-spec user_list(team_id(), integer(), integer(), token()) ->
    {ok, [map()]} | {error, term()}.

user_list(Team, Offset, Limit, T) ->
    get(["/teams/", Team, "/users/", bin(Offset), "/", bin(Limit)], T).


%% @doc
-spec user_list(team_id(), channel_id(), integer(), integer(), token()) ->
    {ok, [map()]} | {error, term()}.

user_list(TeamId, ChId, Offset, Limit, T) ->
    get(["/teams/", TeamId, "/channels/", ChId, "/users/", 
         bin(Offset), "/", bin(Limit)], T).


%% @doc
%% https://api.mattermost.com/#tag/users%2Fpaths%2F~1users~1search%2Fpost

user_search(User, Opts, T) ->
    User2 = bin(User),
    post_raw("/users/search", Opts#{term=>User2}, T).


%% NOT YET
user_get_byname(Name, T) ->
    get(["/users/name/", bin(Name)], T).


%% @private SAYS USER INVALID
user_update(Data, T) ->
    case user_get(T) of
        {ok, User} ->
            post("/users/update", maps:merge(User, Data), T);
        {error, Error} ->
            {error, Error}
    end.




%% ===================================================================
%% Teams
%% ===================================================================


%% @doc
-spec team_list(token()) ->
    {ok, #{team_id() => map()}} | error().

team_list(T) ->
    get("/teams/all", T).


%% @doc
-spec team_get(team_id(), token()) ->
    {ok, map()} | error().

team_get(Id, T) ->
    get(["/teams/", Id, "/me"], T).



%% @private
team_create(Name, Display, Open, T) ->
    Data = #{
        name => bin(Name),
        display_name => bin(Display),
        type => case Open of true -> <<"O">>; false -> <<"I">> end
    },
    post("/teams/create", Data, T).


%% @doc
-spec team_members(team_id(), token()) ->
    {ok, [user_id()]} | error().

team_members(Id, T) ->
    team_members(Id, 0, 100, T).


%% @doc
-spec team_members(team_id(), integer(), integer(), token()) ->
    {ok, [user_id()]} | error().

%% @private
team_members(Id, Offset, Limit, T) ->
    case get(["/teams/", Id, "/members/", bin(Offset), "/", bin(Limit)], T) of
        {ok, List} ->
            List2 = lists:map(
                fun(#{<<"user_id">>:=User, <<"delete_at">>:=_At}) -> User end,
                List),
            {ok, List2};
        {error, Error} ->
            {error, Error}
    end.

%% @doc
-spec team_stats(team_id(), token()) ->
    {ok, Active::integer(), Total::integer()} | error().

team_stats(Id, T) ->
    case get(["/teams/", Id, "/stats"], T) of
        {ok, #{<<"active_member_count">>:=Active, <<"total_member_count">>:=Total}} ->
            {ok, Active, Total};
        {error, Error} ->
            {error, Error}
    end.

%% @private
team_add_user(Team, User, T) ->
    Data = #{user_id => bin(User)},
    post(["/teams/", Team, "/add_user_to_team"], Data, T).


%% @private
team_remove_user(Team, User, T) ->
    Data = #{user_id => bin(User)},
    post(["/teams/", Team, "/remove_user_from_team"], Data, T).






%% ===================================================================
%% Channels
%% ===================================================================


%% @private
-spec channel_list(team_id(), token()) ->
    {ok, [map()]} | {error, error()}.

channel_list(TeamId, T) ->
    get(["/teams/", TeamId, "/channels/"], T).



channel_extra_info(TeamId, ChId, T) ->
    get(["/teams/", TeamId, "/channels/", ChId, "/extra_info"], T).



%% @private
create_channel(TeamId, Name, Display, Open, Opts, T) ->
    Data = Opts#{
        team_id => bin(TeamId),
        name => bin(Name),
        display_name => bin(Display),
        type => case Open of true -> <<"O">>; false -> <<"P">> end
    },
    post(["/teams/", TeamId, "/channels/create"], Data, T).


%% @private
update_channel(TeamId, Name, Display, Open, Opts, T) ->
    Data = Opts#{
        team_id => bin(TeamId),
        name => bin(Name),
        display_name => bin(Display),
        type => case Open of true -> <<"O">>; false -> <<"P">> end
    },
    post(["/teams/", TeamId, "/channels/create"], Data, T).




%% @private Channels we are not member of
get_user_more_channels(TeamId, T) ->
    get(["/teams/", TeamId, "/channels/more"], T).


%% @private NOT WORKING
get_user_channels_members(TeamId, T) ->
    get(["/teams/", TeamId, "/channels/members/"], T).


%% @private NOT WORKING
get_channel(TeamId, ChId, T) ->
    get(["/teams/", TeamId, "/channels/", ChId], T).


%% @private 
channel_add_user(TeamId, ChId, UserId, T) ->
    Data = #{user_id => bin(UserId)},
    post(["/teams/", TeamId, "/channels/", ChId, "/add"], Data, T).



%% ===================================================================
%% Posts
%% ===================================================================

%% @private 
posts_search(TeamId, Terms, IsOr, T) ->
    Data = #{terms=>bin(Terms), is_or_search=>IsOr},
    post(["/teams/", TeamId, "/posts/search"], Data, T).


%% @private Working
posts_create(TeamId, ChId, Data, T) ->
    Data2 = Data#{channel_id=>bin(ChId)},
    post(["/teams/", TeamId, "/channels/", ChId, "/posts/create"], 
          Data2, T).


%% @private WORKING
posts_list(TeamId, ChId, Offset, Limit, T) ->
    get(["/teams/", TeamId, "/channels/", ChId, "/posts/page/",
          Offset, "/", Limit], T).


posts_get(TeamId, ChId, PostId, T) ->
    get(["/teams/", TeamId, "/channels/", ChId, "/posts/", PostId,
          "/get"], T).


%% @private NO PERMISSION
posts_update(TeamId, ChId, PostId, Data, T) ->
    Data2 = Data#{
        id => bin(PostId),
        channel_id => bin(ChId)
    },
    post(["/teams/", TeamId, "/channels/", ChId, "/posts/update"],
          Data2, T).



%% ===================================================================
%% Emoji
%% ===================================================================

emoji_list(T) ->
    get("/emoji/list", T).



%% ===================================================================
%% Internal
%% ===================================================================


%% @private
get(Path, Token) ->
    http(get, Path, <<>>, Token).


%% @private
post(Path, Body, Token) ->
    case post_raw(Path, Body, Token) of
        {ok, _} -> ok;
        {error, Error} -> {error, Error}
    end.


%% @private
post_raw(Path, Body, Token) ->
    http(post, Path, Body, Token).


http(Method, Path, Body, Token) ->
    Url = list_to_binary([?HOST, "/api/v3", Path]),
    lager:info("~p: ~s", [Method, Url]),
    Hds = case Token of 
        <<>> -> 
            [];
        _ -> 
            [{<<"Authorization">>, <<"Bearer ", Token/binary>>}]
    end,
    case nkchat_util:http(Method, Url, Hds, Body) of
        {ok, 200, RespHds, RespBody} when Token == <<>> ->
            Token2 = nklib_util:get_value(<<"Token">>, RespHds),
            {ok, RespBody#{token=>Token2}};
        {ok, 200, _, RespBody} ->
            {ok, RespBody};
        {ok, _, _, #{<<"id">>:=Id, <<"message">>:=Msg}} ->
            {error, {mm_error, Id, Msg}};
        {ok, Code, _, RespBody} ->
            {error, {Code, RespBody}};
        {error, Error} ->
            {error, Error}
    end.








bin(T) ->
    nklib_util:to_binary(T).