-module(host1_handler).

-include_lib("mixer/include/mixer.hrl").
-mixin([
        {example_default,
         [
          init/3,
          content_types_accepted/2,
          content_types_provided/2,
          resource_exists/2
         ]}
       ]).

-export([ rest_init/2
        , allowed_methods/2
        , handle_get/2]).

%trails
-behaviour(trails_handler).
-export([trails/0]).

%% cowboy
rest_init(Req, Opts) ->
  {ok, Req, Opts}.

trails() ->
  Metadata =
    #{get =>
      #{tags => ["whoami"],
        description => "Get hostname",
        produces => ["text/plain"]
      }
    },
  [trails:trail(
    "/whoami",
    host1_handler,
    #{host => "host1"},
    Metadata)].

%% cowboy
allowed_methods(Req, State) ->
  {[<<"GET">>], Req, State}.

%% internal
handle_get(Req, State) ->
  ct:pal("State: ~p~n", [State]),
  Host = maps:get(host, State),
  Body = [<<"I am">> , Host],
  {Body, Req, State}.
