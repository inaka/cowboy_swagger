-module(cowboy_swagger_handler_SUITE).

-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2
]).
-export([
    handler_test/1,
    multiple_hosts_test/1,
    custom_base_path_test/1,
    swagger_json_url_test/1,
    custom_static_files_test/1,
    missing_template_test/1,
    robots_noindex_test/1,
    robots_custom_html_test/1,
    path_validation_test/1,
    startup_path_validation_test/1
]).

-hank([unnecessary_function_arguments]).

-spec all() -> [atom()].
all() ->
    cowboy_swagger_test_utils:all(?MODULE).

-spec init_per_suite(cowboy_swagger_test_utils:config()) ->
    cowboy_swagger_test_utils:config().
init_per_suite(Config) ->
    {ok, _} = shotgun:start(),
    Config.

-spec end_per_suite(cowboy_swagger_test_utils:config()) ->
    cowboy_swagger_test_utils:config().
end_per_suite(Config) ->
    _ = shotgun:stop(),
    Config.

-spec init_per_testcase(
    TestCase :: atom(),
    Config :: cowboy_swagger_test_utils:config()
) ->
    cowboy_swagger_test_utils:config().
init_per_testcase(startup_path_validation_test, Config) ->
    {ok, _} = application:ensure_all_started(cowboy),
    ok = application:set_env(example, validate_paths_on_startup, true),
    Config;
init_per_testcase(custom_base_path_test, Config) ->
    ok = application:set_env(cowboy_swagger, base_path, "/v2/docs"),
    {ok, _} = example:start(),
    Config;
init_per_testcase(Test, Config) when
    Test =:= custom_static_files_test;
    Test =:= missing_template_test;
    Test =:= robots_custom_html_test
->
    StaticFiles = filename:join(proplists:get_value(priv_dir, Config), atom_to_list(Test)),
    ok = file:make_dir(StaticFiles),
    case Test of
        custom_static_files_test ->
            ok = file:write_file(
                filename:join(StaticFiles, "swagger-initializer.template"),
                ~"const url = __SWAGGER_JSON_URL__;"
            ),
            ok = file:write_file(filename:join(StaticFiles, "index.html"), ~"Custom UI");
        robots_custom_html_test ->
            ok;
        missing_template_test ->
            ok
    end,
    ok = application:set_env(cowboy_swagger, static_files, StaticFiles),
    {ok, _} = example:start(),
    Config;
init_per_testcase(Test, Config) when Test =:= swagger_json_url_test; Test =:= robots_noindex_test ->
    {ok, _} = example:start(),
    Config;
init_per_testcase(Test, Config) when Test =:= handler_test; Test =:= path_validation_test ->
    {ok, _} = example:start(),
    Config;
init_per_testcase(multiple_hosts_test, Config) ->
    {ok, _} = multiple_hosts_servers_example:start(),
    Config.

-spec end_per_testcase(
    TestCase :: atom(),
    Config :: cowboy_swagger_test_utils:config()
) ->
    cowboy_swagger_test_utils:config().
end_per_testcase(Test, Config) when Test =/= multiple_hosts_test ->
    _ = example:stop(),
    ok = cleanup(),
    Config;
end_per_testcase(multiple_hosts_test, Config) ->
    _ = multiple_hosts_servers_example:stop(),
    ok = cleanup(),
    Config.

-doc false.
-spec cleanup() -> ok.
cleanup() ->
    ok = application:unset_env(example, validate_paths_on_startup),
    lists:foreach(
        fun(Key) -> ok = application:unset_env(cowboy_swagger, Key) end,
        [base_path, swagger_json_url, static_files, robots_noindex, validate_path_parameters]
    ),
    _ = application:stop(cowboy_swagger),
    _ = application:stop(trails),
    ok.

-spec startup_path_validation_test(cowboy_swagger_test_utils:config()) -> ok.
startup_path_validation_test(_Config) ->
    false = maps:is_key(example_http, ranch:info()),
    %% The explicit startup check rejects the echo fixture's missing GET binding.
    {error, {example, Reason}} = example:start(),
    {_, _} = binary:match(
        iolist_to_binary(io_lib:format("~p", [Reason])),
        ~"missing_path_parameters"
    ),
    false = lists:keymember(example, 1, application:which_applications()),
    false = maps:is_key(example_http, ranch:info()),
    %% Show that this was a validation failure, not a listener/port failure.
    ok = application:set_env(example, validate_paths_on_startup, false),
    {ok, _} = example:start(),
    true = maps:is_key(example_http, ranch:info()),
    #{status_code := 200} = cowboy_swagger_test_utils:api_call(get, "/api-docs/swagger.json"),
    ok.

-spec handler_test(cowboy_swagger_test_utils:config()) -> {atom(), string()}.
handler_test(_Config) ->
    %% Expected result
    Trails =
        trails:trails([
            example_echo_handler,
            example_description_handler,
            cowboy_swagger_handler
        ]),
    SanitizeTrails = cowboy_swagger:filter_cowboy_swagger_handler(Trails),
    ExpectedPaths =
        cowboy_swagger:dec_json(
            cowboy_swagger:enc_json(
                cowboy_swagger:swagger_paths(SanitizeTrails)
            )
        ),

    %% GET swagger.json spec
    ct:comment("GET /api-docs/swagger.json should return 200 OK"),
    #{status_code := 200, body := Body0} =
        cowboy_swagger_test_utils:api_call(get, "/api-docs/swagger.json"),
    #{
        ~"openapi" := ~"3.0.0",
        ~"info" := #{~"title" := ~"Example API"},
        ~"paths" := ExpectedPaths
    } =
        cowboy_swagger:dec_json(Body0),

    %% GET index.html
    ct:comment(
        "GET /api-docs should return 301 MOVED PERMANENTLY to " ++
            "/api-docs/index.html"
    ),
    #{status_code := 301, headers := Headers} =
        cowboy_swagger_test_utils:api_call(get, "/api-docs"),
    Location = {~"location", ~"/api-docs/index.html"},
    Location = lists:keyfind(~"location", 1, Headers),

    %% GET swagger-ui.js - test /api-docs/[...] trail
    ct:comment("GET /api-docs/swagger-ui-js should return 200 OK"),
    #{status_code := 200, body := SwaggerUIBody} =
        cowboy_swagger_test_utils:api_call(get, "/api-docs/swagger-ui.js"),
    {ok, SwaggerUIBodySrc} = file:read_file("../../../../priv/swagger/swagger-ui.js"),
    SwaggerUIBody = SwaggerUIBodySrc,

    %% The initializer must use the default URL without an unresolved placeholder.
    ok = check_initializer("/api-docs", ~"/api-docs/swagger.json"),

    %% GET index.html through the static route
    #{status_code := 200} =
        cowboy_swagger_test_utils:api_call(get, "/api-docs/index.html"),

    %% GET unknown-file.ext - test /api-docs/[...] trail
    ct:comment("GET /api-docs/unknown-file.ext should return 404 NOT FOUND"),
    #{status_code := 404} =
        cowboy_swagger_test_utils:api_call(get, "/api-docs/unknown-file.ext"),
    {comment, ""}.

-spec multiple_hosts_test(_Config :: cowboy_swagger_test_utils:config()) ->
    {atom(), string()}.
multiple_hosts_test(_Config) ->
    %% api1 - host1
    Trails11 = trails:trails(example_echo_handler),
    ExpectedPaths11 = get_expected_paths(Trails11),
    %% GET swagger.json spec from localhost:8383
    ct:comment("GET /api-docs/swagger.json should return 200 OK"),
    #{status_code := 200, body := Body11} =
        cowboy_swagger_test_utils:api_call(get, "/api-docs/swagger.json", "localhost", 8383),
    #{
        ~"openapi" := ~"3.0.0",
        ~"info" := #{~"title" := ~"Example API"},
        ~"paths" := ExpectedPaths11
    } =
        cowboy_swagger:dec_json(Body11),
    %% api1 - host2
    Trails12 = trails:trails(host1_handler),
    ExpectedPaths12 = get_expected_paths(Trails12),
    %% GET swagger.json spec from 127.0.0.1:8383
    ct:comment("GET /api-docs/swagger.json should return 200 OK"),
    #{status_code := 200, body := Body12} =
        cowboy_swagger_test_utils:api_call(get, "/api-docs/swagger.json", "127.0.0.1", 8383),
    #{
        ~"openapi" := ~"3.0.0",
        ~"info" := #{~"title" := ~"Example API"},
        ~"paths" := ExpectedPaths12
    } =
        cowboy_swagger:dec_json(Body12),
    %% api2 - host1
    Trails21 = trails:trails([host1_handler, example_echo_handler]),
    ExpectedPaths21 = get_expected_paths(Trails21),
    %% GET swagger.json spec from localhost:8282
    ct:comment("GET /api-docs/swagger.json should return 200 OK"),
    #{status_code := 200, body := Body21} =
        cowboy_swagger_test_utils:api_call(get, "/api-docs/swagger.json", "localhost", 8282),
    #{
        ~"openapi" := ~"3.0.0",
        ~"info" := #{~"title" := ~"Example API"},
        ~"paths" := ExpectedPaths21
    } =
        cowboy_swagger:dec_json(Body21),
    {comment, ""}.

-doc false.
-spec get_expected_paths(Trails :: trails:trails()) -> json:decode_value().
get_expected_paths(Trails) ->
    SanitizeTrails = cowboy_swagger:filter_cowboy_swagger_handler(Trails),
    cowboy_swagger:dec_json(
        cowboy_swagger:enc_json(
            cowboy_swagger:swagger_paths(SanitizeTrails)
        )
    ).

-spec custom_base_path_test(cowboy_swagger_test_utils:config()) -> ok.
custom_base_path_test(_Config) ->
    #{status_code := 301, headers := Headers} =
        cowboy_swagger_test_utils:api_call(get, "/v2/docs"),
    {~"location", ~"/v2/docs/index.html"} = lists:keyfind(~"location", 1, Headers),
    ok = application:set_env(cowboy_swagger, robots_noindex, true),
    #{status_code := 200, body := HTML} =
        cowboy_swagger_test_utils:api_call(get, "/v2/docs/index.html"),
    ok = check_robots_tag(HTML),
    #{status_code := 200} =
        cowboy_swagger_test_utils:api_call(get, "/v2/docs/swagger-ui.js"),
    #{status_code := 200, body := Body} =
        cowboy_swagger_test_utils:api_call(get, "/v2/docs/swagger.json"),
    #{~"openapi" := ~"3.0.0"} = cowboy_swagger:dec_json(Body),
    #{status_code := 404} =
        cowboy_swagger_test_utils:api_call(get, "/api-docs/swagger.json"),
    ok = check_initializer("/v2/docs", ~"/v2/docs/swagger.json"),
    %% An independent UI source must not move the local spec route.
    ok = application:set_env(cowboy_swagger, swagger_json_url, "/specs/v3.json"),
    ok = check_initializer("/v2/docs", ~"/specs/v3.json"),
    #{status_code := 200} =
        cowboy_swagger_test_utils:api_call(get, "/v2/docs/swagger.json"),
    ok.

-spec swagger_json_url_test(cowboy_swagger_test_utils:config()) -> ok.
swagger_json_url_test(_Config) ->
    %% Change the environment after startup: no recompilation or listener restart.
    lists:foreach(
        fun(URL) ->
            ok = application:set_env(cowboy_swagger, swagger_json_url, URL),
            ok = check_initializer("/api-docs", unicode:characters_to_binary(URL))
        end,
        ["/specs/v1.json", ~"https://example.org/spec.json?name=\"quoted\"&path=\\test"]
    ),
    ok = application:unset_env(cowboy_swagger, swagger_json_url),
    check_initializer("/api-docs", ~"/api-docs/swagger.json").

-spec custom_static_files_test(cowboy_swagger_test_utils:config()) -> ok.
custom_static_files_test(_Config) ->
    #{status_code := 200, body := ~"Custom UI"} =
        cowboy_swagger_test_utils:api_call(get, "/api-docs/index.html"),
    #{status_code := 200, body := ~"const url = \"/api-docs/swagger.json\";"} =
        cowboy_swagger_test_utils:api_call(get, "/api-docs/swagger-initializer.js"),
    ok.

-spec missing_template_test(cowboy_swagger_test_utils:config()) -> ok.
missing_template_test(_Config) ->
    #{status_code := 404} =
        cowboy_swagger_test_utils:api_call(get, "/api-docs/swagger-initializer.js"),
    ok.

-spec check_initializer(string(), binary()) -> ok.
check_initializer(BasePath, URL) ->
    #{status_code := 200, body := Body, headers := Headers} =
        cowboy_swagger_test_utils:api_call(get, BasePath ++ "/swagger-initializer.js"),
    {~"content-type", ~"application/javascript"} = lists:keyfind(~"content-type", 1, Headers),
    nomatch = binary:match(Body, ~"__SWAGGER_JSON_URL__"),
    {_, _} = binary:match(Body, iolist_to_binary(json:encode(URL))),
    ok.

-spec robots_noindex_test(cowboy_swagger_test_utils:config()) -> ok.
robots_noindex_test(_Config) ->
    {ok, Original} = file:read_file("../../../../priv/swagger/index.html"),
    #{status_code := 200, body := Original} =
        cowboy_swagger_test_utils:api_call(get, "/api-docs/index.html"),
    ok = application:set_env(cowboy_swagger, robots_noindex, false),
    #{status_code := 200, body := Original} =
        cowboy_swagger_test_utils:api_call(get, "/api-docs/index.html"),
    ok = application:set_env(cowboy_swagger, robots_noindex, true),
    #{status_code := 200, body := HTML, headers := Headers} =
        cowboy_swagger_test_utils:api_call(get, "/api-docs/index.html"),
    {~"content-type", ~"text/html"} = lists:keyfind(~"content-type", 1, Headers),
    ok = check_robots_tag(HTML),
    #{status_code := 200, body := HTML} =
        cowboy_swagger_test_utils:api_call(get, "/api-docs/index.html"),
    #{status_code := 200, headers := HeadHeaders} =
        HeadResponse =
        cowboy_swagger_test_utils:api_call(head, "/api-docs/index.html"),
    ~"" = maps:get(body, HeadResponse, ~""),
    ContentLength = integer_to_binary(byte_size(HTML)),
    {~"content-length", ContentLength} = lists:keyfind(~"content-length", 1, HeadHeaders),
    %% Enabling the setting must not modify the file or affect other assets.
    {ok, Original} = file:read_file("../../../../priv/swagger/index.html"),
    #{status_code := 200} =
        cowboy_swagger_test_utils:api_call(get, "/api-docs/swagger-ui.css"),
    ok = application:unset_env(cowboy_swagger, robots_noindex),
    #{status_code := 200, body := Original} =
        cowboy_swagger_test_utils:api_call(get, "/api-docs/index.html"),
    ok.

-spec robots_custom_html_test(cowboy_swagger_test_utils:config()) -> ok.
robots_custom_html_test(_Config) ->
    StaticFiles = application:get_env(cowboy_swagger, static_files, ""),
    Index = filename:join(StaticFiles, "index.html"),
    ok = application:set_env(cowboy_swagger, robots_noindex, true),
    #{status_code := 404} =
        cowboy_swagger_test_utils:api_call(get, "/api-docs/index.html"),
    %% Custom HTML can use upper-case tags.
    Original =
        ~"<HTML><HEAD><title>Custom UI</title></HEAD><BODY>Custom UI</BODY></HTML>",
    ok = file:write_file(Index, Original),
    #{status_code := 200, body := HTML} =
        cowboy_swagger_test_utils:api_call(get, "/api-docs/index.html"),
    ok = check_robots_tag(HTML),
    %% Documents without a head are passed through unchanged.
    Headless = ~"<div>Custom UI fragment</div>",
    ok = file:write_file(Index, Headless),
    #{status_code := 200, body := Headless} =
        cowboy_swagger_test_utils:api_call(get, "/api-docs/index.html"),
    ok.

-doc false.
-spec check_robots_tag(binary()) -> ok.
check_robots_tag(HTML) ->
    Tag = ~"<meta name=\"robots\" content=\"noindex, nofollow\" />",
    [_, _] = binary:split(HTML, Tag, [global]),
    {match, _} = re:run(
        HTML,
        ~"<head[^>]*>.*<meta name=\"robots\" content=\"noindex, nofollow\" />.*</head>",
        [caseless, dotall]
    ),
    ok.

-spec path_validation_test(cowboy_swagger_test_utils:config()) -> ok.
path_validation_test(_Config) ->
    %% The existing echo fixture omits its optional binding's GET parameter.
    #{status_code := 200} =
        cowboy_swagger_test_utils:api_call(get, "/api-docs/swagger.json"),
    ok = application:set_env(cowboy_swagger, validate_path_parameters, true),
    #{status_code := 500} =
        cowboy_swagger_test_utils:api_call(get, "/api-docs/swagger.json"),
    ok = application:unset_env(cowboy_swagger, validate_path_parameters),
    #{status_code := 200} =
        cowboy_swagger_test_utils:api_call(get, "/api-docs/swagger.json"),
    ok.
