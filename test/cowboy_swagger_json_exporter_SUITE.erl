-module(cowboy_swagger_json_exporter_SUITE).

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([disabled_test/1, enabled_test/1, file_export_test/1, failure_test/1]).

-hank([unnecessary_function_arguments]).

-spec all() -> [atom()].
all() ->
    cowboy_swagger_test_utils:all(?MODULE).

-spec init_per_testcase(atom(), cowboy_swagger_test_utils:config()) ->
    cowboy_swagger_test_utils:config().
init_per_testcase(_Test, Config) ->
    Keys = [json_exporter, test_exporter_result, global_spec, server_spec],
    Previous = [{Key, application:get_env(cowboy_swagger, Key)} || Key <:- Keys],
    ok = application:unset_env(cowboy_swagger, json_exporter),
    ok = application:unset_env(cowboy_swagger, test_exporter_result),
    ok = cowboy_swagger:set_global_spec(#{
        openapi => ~"3.0.0", info => #{title => ~"Exported API", version => ~"1.0.0"}
    }),
    ok = application:set_env(cowboy_swagger, server_spec, #{}),
    [{previous_env, Previous} | Config].

-spec end_per_testcase(atom(), cowboy_swagger_test_utils:config()) ->
    cowboy_swagger_test_utils:config().
end_per_testcase(_Test, Config) ->
    lists:foreach(
        fun
            ({Key, undefined}) -> ok = application:unset_env(cowboy_swagger, Key);
            ({Key, {ok, Value}}) -> ok = application:set_env(cowboy_swagger, Key, Value)
        end,
        proplists:get_value(previous_env, Config)
    ),
    Config.

-spec disabled_test(cowboy_swagger_test_utils:config()) -> ok.
disabled_test(_Config) ->
    %% No running application, HTTP listener or exporter is needed.
    Trails = trails:trails(example_description_handler),
    JSON = cowboy_swagger:to_json(Trails),
    JSON = cowboy_swagger:to_json(export_test_server, Trails),
    #{~"paths" := #{~"/description" := _}} = json:decode(JSON),
    no_extra_exports().

-spec enabled_test(cowboy_swagger_test_utils:config()) -> ok.
enabled_test(_Config) ->
    Trails = trails:trails(example_description_handler),
    Expected = cowboy_swagger:to_json(Trails),
    ok = application:set_env(cowboy_swagger, json_exporter, cowboy_swagger_test_exporter),
    Expected = cowboy_swagger:to_json(Trails),
    ok = exported_once(Expected),
    ok = cowboy_swagger:set_server_spec(export_test_server, #{
        openapi => ~"3.0.0", info => #{title => ~"Server API", version => ~"2.0.0"}
    }),
    ServerJSON = cowboy_swagger:to_json(export_test_server, Trails),
    #{~"info" := #{~"title" := ~"Server API"}} = json:decode(ServerJSON),
    ok = exported_once(ServerJSON),
    ok = application:unset_env(cowboy_swagger, json_exporter),
    Expected = cowboy_swagger:to_json(Trails),
    no_extra_exports().

-spec file_export_test(cowboy_swagger_test_utils:config()) -> ok.
file_export_test(Config) ->
    Path = filename:join(proplists:get_value(priv_dir, Config), "swagger.json"),
    ok = application:set_env(cowboy_swagger, json_exporter, cowboy_swagger_test_exporter),
    ok = application:set_env(cowboy_swagger, test_exporter_result, {file, Path}),
    JSON = cowboy_swagger:to_json(trails:trails(example_description_handler)),
    {ok, JSON} = file:read_file(Path),
    #{~"openapi" := ~"3.0.0", ~"paths" := #{~"/description" := _}} = json:decode(JSON),
    exported_once(JSON).

-spec failure_test(cowboy_swagger_test_utils:config()) -> ok.
failure_test(Config) ->
    Trails = trails:trails(example_description_handler),
    ok = application:set_env(cowboy_swagger, json_exporter, cowboy_swagger_test_exporter),
    Path = filename:join([proplists:get_value(priv_dir, Config), "missing", "swagger.json"]),
    ok = application:set_env(cowboy_swagger, test_exporter_result, {file, Path}),
    try cowboy_swagger:to_json(Trails) of
        _ -> ct:fail(expected_export_failure)
    catch
        error:{badmatch, {error, enoent}}:WriteStack ->
            [{cowboy_swagger_test_exporter, export, 1, _} | _] = WriteStack
    end,
    receive
        {exported_json, _} -> ok
    after 1000 -> ct:fail(export_not_called)
    end,
    ok = application:set_env(cowboy_swagger, test_exporter_result, crash),
    try cowboy_swagger:to_json(Trails) of
        _ -> ct:fail(expected_exporter_exception)
    catch
        error:exporter_crashed:CrashStack ->
            [{cowboy_swagger_test_exporter, export, 1, _} | _] = CrashStack
    end,
    receive
        {exported_json, _} -> ok
    after 1000 -> ct:fail(export_not_called)
    end,
    no_extra_exports().

-doc false.
-spec exported_once(binary()) -> ok.
exported_once(JSON) ->
    receive
        {exported_json, JSON} -> ok
    after 1000 ->
        ct:fail(export_not_called)
    end,
    no_extra_exports().

-doc false.
-spec no_extra_exports() -> ok.
no_extra_exports() ->
    receive
        {exported_json, _} -> ct:fail(unexpected_export)
    after 0 ->
        ok
    end.
