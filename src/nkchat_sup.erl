-module(nkchat_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).


start_link() ->
    Childs = [
    ],
    supervisor:start_link({local, ?MODULE}, ?MODULE, {{one_for_one, 10, 60}, Childs}).


%% @private
init(ChildSpecs) ->
    {ok, ChildSpecs}.


