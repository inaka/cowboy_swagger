-module(example_json_exporter).
-moduledoc """
Writes a specification to the file selected by the build export script.
""".

-behaviour(cowboy_swagger_json_exporter).

-export([export/1]).

-spec export(binary()) -> ok.
export(JSON) ->
    Path = application:get_env(example, swagger_output, "_build/swagger.json"),
    ok = filelib:ensure_dir(Path),
    ok = file:write_file(Path, JSON).
