-module(nkchat_app).
-behaviour(application).

-export([start/0, start/2, stop/1]).
-export([get/1, get/2, get_srv/2, put/2]).

-define(APP, nkchat).


%% doc Start manually
start() ->
    case nklib_util:ensure_all_started(?APP, permanent) of
        {ok, _Started} ->
            ok;
        Error ->
            Error
    end.


%% @private OTP standard start callback
start(_Type, _Args) ->
	Syntax = #{},
    case nklib_config:load_env(?APP, Syntax) of
        {ok, _} ->
            {ok, Pid} = nkchat_sup:start_link(),
            {ok, Vsn} = application:get_key(?APP, vsn),
            lager:info("NkCHAT v~s has started.", [Vsn]),
            register_types(),
            {ok, Pid};
        {error, Error} ->
            lager:error("Config error: ~p", [Error]),
            error(config_error)
    end.



%% @private OTP standard stop callback
stop(_) ->
    ok.


%% @private
register_types() ->
    ok = nkdomain_types:register_type(nkchat_conversation, 'chat.conversation').


%% Config Management
get(Key) ->
    nklib_config:get(?APP, Key).

get(Key, Default) ->
    nklib_config:get(?APP, Key, Default).

get_srv(Class, Key) ->
    nklib_config:get_domain(?APP, Class, Key).

put(Key, Val) ->
    nklib_config:put(?APP, Key, Val).
