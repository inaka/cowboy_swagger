-module(example_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-spec start_link() -> supervisor:startlink_ret().
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, {}).

-spec init({}) -> {ok, {{one_for_one, 5, 10}, []}}.
init({}) ->
    {ok, {{one_for_one, 5, 10}, []}}.
