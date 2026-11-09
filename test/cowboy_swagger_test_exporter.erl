-module(cowboy_swagger_test_exporter).
-moduledoc false.

-behaviour(cowboy_swagger_json_exporter).

-export([export/1]).

-spec export(binary()) -> ok.
export(JSON) ->
    self() ! {exported_json, JSON},
    case application:get_env(cowboy_swagger, test_exporter_result, ok) of
        {file, Path} -> ok = file:write_file(Path, JSON);
        crash -> error(exporter_crashed);
        ok -> ok
    end.
