-module(cowboy_swagger_handler_SUITE).

%% CT
-export([ all/0
        , init_per_suite/1
        , end_per_suite/1
        ]).

%% Test cases
-export([handler_test/1,
         multiple_hosts_test/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Common test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec all() -> [atom()].
all() ->
  cowboy_swagger_test_utils:all(?MODULE).

-spec init_per_suite(
  cowboy_swagger_test_utils:config()
) -> cowboy_swagger_test_utils:config().
init_per_suite(Config) ->
  {ok, _} = shotgun:start(),
  {ok, _} = example:start(),
  {ok, _} = multiple_hosts_servers_example:start(),
  Config.

-spec end_per_suite(
  cowboy_swagger_test_utils:config()
) -> cowboy_swagger_test_utils:config().
end_per_suite(Config) ->
  _ = shotgun:stop(),
  _ = example:stop(),
  _ = multiple_hosts_servers_example:stop(),
  Config.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test Cases
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec handler_test(cowboy_swagger_test_utils:config()) -> {atom(), string()}.
handler_test(_Config) ->
  %% Expected result
  Trails = trails:trails([example_echo_handler,
                          example_description_handler,
                          cowboy_swagger_handler,
                          host1_handler]),
  SanitizeTrails = cowboy_swagger:filter_cowboy_swagger_handler(Trails),
  ExpectedPaths = cowboy_swagger:dec_json(
    cowboy_swagger:enc_json(cowboy_swagger:swagger_paths(SanitizeTrails))),

  %% GET swagger.json spec
  ct:comment("GET /api-docs/swagger.json should return 200 OK"),
  #{status_code := 200, body := Body0} =
    cowboy_swagger_test_utils:api_call(get, "/api-docs/swagger.json"),
  #{<<"swagger">> := <<"2.0">>,
    <<"info">> := #{<<"title">> := <<"Example API">>},
    <<"paths">> := ExpectedPaths} = cowboy_swagger:dec_json(Body0),

  %% GET index.html
  ct:comment("GET /api-docs should return 200 OK with the index.html"),
  #{status_code := 200, body := Body1} =
    cowboy_swagger_test_utils:api_call(get, "/api-docs"),
  {ok, Index} = file:read_file("../../priv/swagger/index.html"),
  Index = Body1,

  %% GET swagger-ui.js - test /api-docs/[...] trail
  ct:comment("GET /api-docs/swagger-ui-js should return 200 OK"),
  #{status_code := 200, body := SwaggerUIBody} =
    cowboy_swagger_test_utils:api_call(get, "/api-docs/swagger-ui.js"),
  {ok, SwaggerUIBodySrc} = file:read_file("../../priv/swagger/swagger-ui.js"),
  SwaggerUIBody = SwaggerUIBodySrc,

  %% GET unknown-file.ext - test /api-docs/[...] trail
  ct:comment("GET /api-docs/unknown-file.ext should return 404 NOT FOUND"),
  #{status_code := 404} =
    cowboy_swagger_test_utils:api_call(get, "/api-docs/unknown-file.ext"),
  {comment, ""}.

-spec multiple_hosts_test(_Config::cowboy_swagger_test_utils:config()) ->
  {atom(), string()}.
multiple_hosts_test(_Config) ->
  %% GET /whoami
  ct:comment("GET /whoami on host1 should return \"I am host1\" as its Body"),
  #{status_code := 200, body := Host1Body} =
    cowboy_swagger_test_utils:api_call(get,
                                       "/whoami",
                                       #{},
                                       [],
                                       multiple_hosts_servers_example,
                                       api1),
  Host1Body = "I am host1",
  {comment, ""}.
