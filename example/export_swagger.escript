#!/usr/bin/env escript
-mode(compile).

main([BuildDir, ConfigPath, OutputPath]) ->
    try
        Paths =
            filelib:wildcard(filename:join(BuildDir, "lib/*/ebin")) ++
                filelib:wildcard(filename:join(BuildDir, "checkouts/*/ebin")),
        ok = code:add_paths(Paths),
        {ok, [Config]} = file:consult(ConfigPath),
        ok = application:set_env(Config, []),
        ok = application:set_env(example, swagger_output, OutputPath),
        ok = application:set_env(cowboy_swagger, json_exporter, example_json_exporter),
        Trails = example:trails(),
        ok = cowboy_swagger:validate_paths(Trails),
        JSON = cowboy_swagger:to_json(Trails),
        {ok, JSON} = file:read_file(OutputPath),
        io:format("Exported Swagger specification to ~ts~n", [OutputPath])
    catch
        Class:Reason:Stack ->
            io:format(standard_error, "Swagger export failed: ~p:~p~n~p~n", [Class, Reason, Stack]),
            halt(1)
    end;
main(_) ->
    io:format(standard_error,
        "Usage: escript ~ts BUILD_DIR SYS_CONFIG OUTPUT_JSON~n", [escript:script_name()]),
    halt(2).
