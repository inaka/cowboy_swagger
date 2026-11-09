#!/usr/bin/env escript
-mode(compile).

main([BuildDir, ConfigPath, OutputPath]) ->
    try
        Paths = lists:append([
            filelib:wildcard(filename:join([BuildDir, Dir, "*", "ebin"]))
         || Dir <:- ["lib", "checkouts"]
        ]),
        ok = code:add_paths(Paths),
        {ok, [Config]} = file:consult(ConfigPath),
        lists:foreach(fun load_config/1, Config),
        ok = application:set_env(example, swagger_output, OutputPath),
        ok = application:set_env(cowboy_swagger, json_exporter_mod, example_json_exporter),
        Trails = trails:trails([example_echo_handler, example_description_handler]),
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
        "Usage: escript export_swagger.escript BUILD_DIR SYS_CONFIG OUTPUT_JSON~n", []),
    halt(2).

load_config({App, Settings}) ->
    lists:foreach(
        fun({Key, Value}) -> ok = application:set_env(App, Key, Value) end,
        Settings
    ).
