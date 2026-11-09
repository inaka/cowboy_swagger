-module(cowboy_swagger_test_exporter).
-moduledoc false.

-behaviour(cowboy_swagger_json_exporter).

-export([export/1]).

-spec export(binary()) -> ok | {error, term()}.
export(JSON) ->
    self() ! {exported_json, JSON},
    case application:get_env(cowboy_swagger, test_exporter_result, ok) of
        {file, Path} -> file:write_file(Path, JSON);
        crash -> error(exporter_crashed);
        Result -> Result
    end.
