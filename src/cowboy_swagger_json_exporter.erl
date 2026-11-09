-module(cowboy_swagger_json_exporter).
-moduledoc """
Callback for exporting a generated Swagger/OpenAPI JSON specification.

Set the `cowboy_swagger` application setting `json_exporter_mod` to a module
implementing this behaviour. Its `export/1` callback receives the serialized
JSON binary whenever `cowboy_swagger:to_json/1,2` generates a specification.
The callback runs synchronously in the caller's process, including calls made
by the HTTP JSON handler. No exporter is configured by default.

Return `ok` after exporting successfully. Returning `{error, Reason}` makes
generation fail with `{json_export_failed, Module, Reason}`. Exceptions raised
by the callback propagate to the caller. Exporters should not call `to_json`
themselves, as that would recursively invoke the callback.

This callback does not schedule generation. To export during a build, invoke
`to_json` from an opt-in post-compile hook after loading the application's
settings and collecting its trails. Neither an HTTP listener nor a running
application is required by the generator itself.
""".

-export([maybe_export/1]).

-doc """
Exports the serialized specification. Output location and storage are chosen
by the implementing module.
""".
-callback export(JSON :: binary()) -> ok | {error, term()}.

-doc false.
-spec maybe_export(binary()) -> ok.
maybe_export(JSON) ->
    case application:get_env(cowboy_swagger, json_exporter_mod) of
        undefined ->
            ok;
        {ok, Module} ->
            case Module:export(JSON) of
                ok -> ok;
                {error, Reason} -> error({json_export_failed, Module, Reason})
            end
    end.
