-module(cowboy_swagger_handler_SUITE).

%% CT
-export([ all/0
        , init_per_suite/1
        , end_per_suite/1
        ]).

%% Test cases
-export([handler_test/1]).

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
  shotgun:start(),
  example:start(),
  Config.

-spec end_per_suite(
  cowboy_swagger_test_utils:config()
) -> cowboy_swagger_test_utils:config().
end_per_suite(Config) ->
  shotgun:stop(),
  example:stop(),
  Config.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test Cases
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec handler_test(cowboy_swagger_test_utils:config()) -> {atom(), string()}.
handler_test(_Config) ->
  %% Expected result
  Trails = trails:trails([example_echo_handler,
                          example_description_handler,
                          cowboy_swagger_handler]),
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

  {comment, ""}.
