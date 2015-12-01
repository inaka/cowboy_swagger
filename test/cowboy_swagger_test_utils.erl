-module(cowboy_swagger_test_utils).

-export([ all/1
        , init_per_suite/1
        , end_per_suite/1
        ]).
-export([ api_call/2
        , api_call/3
        , api_call/4
        , api_call/5
        , api_call/6
        ]).

-type config() :: proplists:proplist().
-export_type([config/0]).

-spec all(atom()) -> [atom()].
all(Module) ->
  ExcludedFuns = [module_info, init_per_suite, end_per_suite, group, all],
  Exports = apply(Module, module_info, [exports]),
  [F || {F, 1} <- Exports, not lists:member(F, ExcludedFuns)].

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
  Config.

-spec end_per_suite(config()) -> config().
end_per_suite(Config) ->
  Config.

-spec api_call(atom(), string()) -> #{}.
api_call(Method, Uri) ->
  api_call(Method, Uri, #{}).

-spec api_call(atom(), string(), #{}) -> #{}.
api_call(Method, Uri, Headers) ->
  api_call(Method, Uri, Headers, []).

-spec api_call(atom(), string(), #{}, iodata()) -> #{}.
api_call(Method, Uri, Headers, Body) ->
  api_call(Method, Uri, Headers, Body, 'example').

api_call(Method, Uri, Headers, Body, AppConfig) ->
  api_call(Method, Uri, Headers, Body, AppConfig, 'http_port').

api_call(Method, Uri, Headers, Body, App, ConfigKey) ->
  {ok, Pid} = case App of
    example ->
      Port = application:get_env(example, ConfigKey, 8080),
      shotgun:open("localhost", Port);
    _ ->
      #{host := HostMatch, port := Port} =
        application:get_env(App, ConfigKey, #{}),
      shotgun:open(HostMatch, Port)
  end,
  try
    {ok, Response} = shotgun:request(Pid, Method, Uri, Headers, Body, #{}),
    Response
  after
    shotgun:close(Pid)
  end.
