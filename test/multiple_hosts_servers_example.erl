-module(multiple_hosts_servers_example).

-export([start/0]).
-export([start/2]).
-export([stop/0]).
-export([stop/1]).
-export([start_phase/3]).

%% application
%% @doc Starts the application
start() ->
  application:ensure_all_started(multiple_hosts_servers_example).

%% @doc Stops the application
stop() ->
  application:stop(multiple_hosts_servers_example).

%% behaviour
%% @private
start(_StartType, _StartArgs) ->
  multiple_hosts_servers_example_sup:start_link().

%% @private
stop(_State) ->
  ok = cowboy:stop_listener(multiple_hosts_servers_http).

-spec start_phase(atom(), application:start_type(), []) -> ok | {error, term()}.
start_phase(start_multiple_hosts_servers_example_http, _StartType, []) ->
  %% Host1
  {ok, #{host := HostMatch1, port := Port1}} =
    application:get_env(multiple_hosts_servers_example, api1),
  %Trails1 = [{HostMatch1, trails:trails([host1_handler])}],
  Trails1 = trails:trails([host1_handler]) ++
            cowboy_swagger_handler:trails(#{host => HostMatch1}),
  trails:store([{HostMatch1, Trails1}]),
  ct:pal("Host1 trails: ~p~n", [trails:all("host1")]),
  Dispatch1 = trails:compile(Trails1),
  RanchOptions1 = [{port, Port1}],
  CowboyOptions1 =
    [
     {env,
      [
       {dispatch, Dispatch1}
      ]},
     {compress, true},
     {timeout, 12000}
    ],
  %% Host2
  %#{server := Server2, host := HostMatch2, port := Port2} =
  %  application:get_env(multiple_hosts_servers_example, api2),
  %Trails2 = [{HostMatch2, trails:trails([host2_handler])}],
  %trails:store(Trails2),
  %Dispatch2 = trails:compile(Trails2),
  %RanchOptions2 = [{port, Port2}],

  {ok, ListenerCount} = application:get_env(multiple_hosts_servers_example,
                                            http_listener_count),
  {ok, _} =
    cowboy:start_http(multiple_hosts_servers_http, ListenerCount, RanchOptions1, CowboyOptions1),
  ok.
