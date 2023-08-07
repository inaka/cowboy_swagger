-module(example).

-export([start/0]).
-export([start/2]).
-export([stop/0]).
-export([stop/1]).
-export([start_phase/3]).

-hank([unnecessary_function_arguments]).

%% application
%% @doc Starts the application
start() ->
    application:ensure_all_started(example).

%% @doc Stops the application
stop() ->
    application:stop(example).

%% behaviour
%% @private
start(_StartType, _StartArgs) ->
    example_sup:start_link().

%% @private
stop(_State) ->
    ok = cowboy:stop_listener(example_http).

-spec start_phase(atom(), application:start_type(), []) -> ok.
start_phase(start_trails_http, _StartType, []) ->
    {ok, Port} = application:get_env(example, http_port),
    Trails =
        trails:trails([example_echo_handler,
                       example_description_handler,
                       cowboy_swagger_handler]),
    trails:store(Trails),
    Dispatch = trails:single_host_compile(Trails),
    RanchOptions = [{port, Port}],
    CowboyOptions =
        #{env => #{dispatch => Dispatch},
          compress => true,
          timeout => 12000},

    {ok, _} = cowboy:start_clear(example_http, RanchOptions, CowboyOptions),
    ok.
