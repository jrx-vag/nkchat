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

-module(nkchat_mm_api).
-compile([export_all]).

-define(HOST, <<"https://mde.netc.io:9443">>).



%% ===================================================================
%% Users
%% ===================================================================

%% @private
login() ->
    login("carlosj", "carlos12").


%% @private
login(User, Pass) ->
    Data = #{
        login_id => bin(User),
        password => bin(Pass)
    },
    http("/users/login", Data).


%% @private
logout() ->
    http("/users/logout", <<>>).


%% @private      
get_user() ->
    http("/users/me").


%% @private      
user_list() ->
    user_list(0, 1000).


%% @private NOT WORKING
user_list(Offset, Limit) ->
    http(["/users/", Offset, "/", Limit, "/"]).


%% @private NOT WORKING
team_user_list(Team, Offset, Limit) ->
    http(["/teams/", Team, "/users/", Offset, "/", Limit]).


%% @private NOT WORKING
search_users(User, Opts) ->
    User2 = bin(User),
    http("/users/search", Opts#{term=>User2}).


%% @private NOT WORKING
channel_user_list(TeamId, ChId, Offset, Limit) ->
    http(["/teams/", TeamId, "/channels/", ChId, "/users/", Offset, 
          "/", Limit]).


%% @private SAYS USER INVALID
user_update(UserId, Data) ->
    Data2 = Data#{
        user_id => bin(UserId)
    },
    http("/users/update", Data2).


%% @private
user_set_pass(UserId, OldPass, NewPass) ->
    Data = #{
        user_id => bin(UserId),
        current_password => bin(OldPass),
        new_password => bin(NewPass)
    },
    http("/users/newpassword", Data).


%% ===================================================================
%% Teams
%% ===================================================================


get_team(Id) ->
    http(["/teams/", Id, "/me"]).


%% @private
get_teams() ->
    http("/teams/all").


%% @private
create_team(Name, Display, Open) ->
    Data = #{
        name => bin(Name),
        display_name => bin(Display),
        type => case Open of true -> <<"O">>; false -> <<"I">> end
    },
    http("/teams/create", Data).


%% @private NOT WORKING
team_members(Id, Offset, Limit) ->
    http(["/teams/", Id, "/members/", Offset, "/", Limit]).


%% @private NOT WORKING
team_stats(Id) ->
    http(["/teams/", Id, "/stats"]).


%% @private
add_to_team(Team, User) ->
    Data = #{user_id => bin(User)},
    http(["/teams/", Team, "/add_user_to_team"], Data).


%% @private
remove_from_team(Team, User) ->
    Data = #{user_id => bin(User)},
    http(["/teams/", Team, "/remove_user_from_team"], Data).






%% ===================================================================
%% Channels
%% ===================================================================


channel_extra_info(TeamId, ChId) ->
    http(["/teams/", TeamId, "/channels/", ChId, "/extra_info"]).



%% @private
create_channel(TeamId, Name, Display, Open, Opts) ->
    Data = Opts#{
        team_id => bin(TeamId),
        name => bin(Name),
        display_name => bin(Display),
        type => case Open of true -> <<"O">>; false -> <<"P">> end
    },
    http(["/teams/", TeamId, "/channels/create"], Data).


%% @private
update_channel(TeamId, Name, Display, Open, Opts) ->
    Data = Opts#{
        team_id => bin(TeamId),
        name => bin(Name),
        display_name => bin(Display),
        type => case Open of true -> <<"O">>; false -> <<"P">> end
    },
    http(["/teams/", TeamId, "/channels/create"], Data).


%% @private
get_user_channels(TeamId) ->
    http(["/teams/", TeamId, "/channels/"]).


%% @private Channels we are not member of
get_user_more_channels(TeamId) ->
    http(["/teams/", TeamId, "/channels/more"]).


%% @private NOT WORKING
get_user_channels_members(TeamId) ->
    http(["/teams/", TeamId, "/channels/members/"]).


%% @private NOT WORKING
get_channel(TeamId, ChId) ->
    http(["/teams/", TeamId, "/channels/", ChId]).


%% @private 
channel_add_user(TeamId, ChId, UserId) ->
    Data = #{user_id => bin(UserId)},
    http(["/teams/", TeamId, "/channels/", ChId, "/add"], Data).



%% ===================================================================
%% Posts
%% ===================================================================

%% @private 
posts_search(TeamId, Terms, IsOr) ->
    Data = #{terms=>bin(Terms), is_or_search=>IsOr},
    http(["/teams/", TeamId, "/posts/search"], Data).


%% @private Working
posts_create(TeamId, ChId, Data) ->
    Data2 = Data#{channel_id=>bin(ChId)},
    http(["/teams/", TeamId, "/channels/", ChId, "/posts/create"], 
          Data2).


%% @private WORKING
posts_list(TeamId, ChId, Offset, Limit) ->
    http(["/teams/", TeamId, "/channels/", ChId, "/posts/page/",
          Offset, "/", Limit]).


posts_get(TeamId, ChId, PostId) ->
    http(["/teams/", TeamId, "/channels/", ChId, "/posts/", PostId,
          "/get"]).


%% @private NO PERMISSION
posts_update(TeamId, ChId, PostId, Data) ->
    Data2 = Data#{
        id => bin(PostId),
        channel_id => bin(ChId)
    },
    http(["/teams/", TeamId, "/channels/", ChId, "/posts/update"],
          Data2).



%% ===================================================================
%% Emoji
%% ===================================================================

emoji_list() ->
    http("/emoji/list").



%% ===================================================================
%% Internal
%% ===================================================================


%% @private
http(Path) ->
    http(get, Path, <<>>).


%% @private
http(Path, Body) ->
    http(post, Path, Body).


%% @private
http(Method, Path, Body) ->
    Body2 = case is_map(Body) of
        true -> nklib_json:encode(Body);
        false -> Body
    end,
    Hds = case nklib_store:get({?MODULE, token}) of
        not_found ->
            % lager:warning("NOT Added token for ~s", [Path]),
            [];
        Token -> 
            % lager:warning("Added token for ~s", [Path]),
            [{<<"Authorization">>, <<"Bearer ", Token/binary>>}]
    end,
    Path2 = case Path of
        [First|_] when is_list(First); is_binary(First) -> 
            [bin(T) || T <- Path];
        _ ->
            Path
    end,
    Url = list_to_binary([?HOST, "/api/v3", Path2]),
    case Method of 
        get -> lager:info("get ~s", [Url]);
        post -> lager:info("post ~s:\n~s", [Url, Body2])
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
    case hackney:request(Method, Url, Hds, Body2, HttpOpts) of
        {ok, Code, RHds, RBody} ->
            Time = (nklib_util:l_timestamp() - Start) div 1000,
            RBody2 = case 
                nklib_util:get_value(<<"Content-Type">>, RHds)
            of
                <<"application/json">> -> nklib_json:decode(RBody);
                _ -> RBody
            end,
            case Code of
                200 -> 
                    case nklib_util:get_value(<<"Token">>, RHds) of
                        undefined -> 
                            ok;
                        Token2 -> 
                            nklib_store:put({?MODULE, token}, Token2)
                    end,
                    {ok, RBody2, Time};
                _ ->
                    {error, {http_code, Code, RBody2}}
            end;
        {error, Error} ->
            {error, Error}
    end.


bin(T) ->
    nklib_util:to_binary(T).