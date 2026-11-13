# Cowboy Swagger OpenAPI 3 example

This Erlang application shows how to describe Cowboy routes, generate Swagger UI
and implement Bearer authorization for an endpoint.

## Build and run

From the example directory, with Erlang/OTP 28+ and rebar3 in PATH:

```sh
rebar3 release
_build/default/rel/example/bin/example console
```

Open http://localhost:8080/api-docs.

To run in the background and stop it afterward:

```sh
_build/default/rel/example/bin/example daemon
_build/default/rel/example/bin/example stop
```

## Endpoints

| Method | Path | Behavior |
| --- | --- | --- |
| GET | `/message` | Read the current message. |
| PUT | `/message/{echo}` | Store the message supplied in the path. |
| GET | `/description` | Read the API description. |
| GET | `/protected` | Return an authenticated response when the Bearer token matches. |

GET and PUT use separate routes with the required path parameters described in
the metadata. In the terminal:

```sh
curl -X PUT http://localhost:8080/message/hello-world
curl http://localhost:8080/message
```

## Authorization in Swagger UI

1. Expand `GET /protected` and select **Try it out → Execute**. Without a token,
   the response is HTTP 401.
2. Select **Authorize**, enter `example-token` without the `Bearer ` prefix,
   then authorize and close the dialog.
3. Execute `/protected` again. The response is HTTP 200 with
   `{"authenticated":true}`.
4. Log out through the Authorize dialog. The endpoint returns HTTP 401 again.

The other endpoints do not require authorization. The same requests can be made
from the terminal:

```sh
curl -i http://localhost:8080/protected
curl -i -H 'Authorization: Bearer incorrect' http://localhost:8080/protected
curl -i -H 'Authorization: Bearer example-token' http://localhost:8080/protected
```

The token is configured in `config/sys.config`:

```erlang
[
    {example, [
        {http_port, 8080},
        {bearer_token, ~"example-token"}
    ]}
].
```

## How the code is connected

- [example_auth_handler.erl](src/example_auth_handler.erl) documents the
  protected operation and implements Cowboy REST's `is_authorized/2` callback.
  It reads the Authorization header and compares the Bearer token with the
  configured value.
- [config/sys.config](config/sys.config) defines the OpenAPI HTTP Bearer security
  scheme. The protected operation refers to it through its `security` metadata,
  making the Authorize control available in Swagger UI.
- [example_echo_handler.erl](src/example_echo_handler.erl) describes the public
  GET and PUT operations and their responses.
- [example.erl](src/example.erl) collects the routes through `trails/0`, validates
  their path parameters and starts Cowboy. The JSON export script uses the same
  routes.

## Build-time JSON export

To generate the example's OpenAPI JSON during compilation:

```sh
rebar3 as swagger compile
```

The opt-in `swagger` profile runs a post-compile hook that writes
`_build/swagger/swagger.json`. Normal `rebar3 compile` and `rebar3 release`
do not enable this hook.

`export_swagger.escript` loads the compiled example and dependency modules,
reads `config/sys.config`, collects the shared routes through `example:trails/0`,
validates them and calls `cowboy_swagger:to_json/1`. It configures `example_json_exporter`
inside the export VM. The exporter creates the output directory and writes
the generated JSON. The example application, Cowboy and HTTP listeners are
not started.

After compiling the `swagger` profile, the script can also be run directly:

```sh
escript export_swagger.escript \
    _build/swagger config/sys.config _build/swagger/swagger.json
```

The arguments are the profile build directory, the configuration file and the
output JSON path. Configuration must be a plain Erlang list of application
settings; the script does not expand configuration includes or environment
variables. Export errors result in a nonzero exit status and fail the build.

The existing `rebar.config.script` prepares the local cowboy_swagger checkout
automatically. Repeated builds preserve an existing checkout instead of
creating nested links inside it.

## Output paths

The hook uses the standard `_build/swagger` directory explicitly because it
runs in the `swagger` profile. These paths are relative to the `example`
directory. If you change the profile or rebar3's build directory, update the
hook's first argument and output path accordingly. You can also pass an
absolute output path to the script.

## Including the JSON in a release

The generated `_build/swagger/swagger.json` remains a build artifact until
you explicitly package it. A normal `rebar3 release` neither enables the
export profile nor copies this file into the release.

In `example/rebar.config`, extend the existing `relx` build configuration's
`overlay` list as shown below. Keep the other existing `relx` options and the
Swagger UI asset overlay:

```erlang
{relx, [
    %% Keep the existing release, sys_config and other relx options here.
    {overlay, [
        {copy, "./_checkouts/cowboy_swagger/priv/swagger", "priv/swagger"},
        {copy, "_build/swagger/swagger.json", "swagger.json"}
    ]}
]}.
```

Then generate a fresh specification before assembling the release:

```sh
rebar3 as swagger compile
rebar3 release
```

With that additional overlay, the JSON is copied to
`_build/default/rel/example/swagger.json`. This overlay is an optional
configuration example and is not enabled in the repository's `rebar.config`.
The generation and release steps should use the intended application settings
and compatible build profiles so the packaged JSON describes the released API.

Alternatively, export into `priv/swagger.json` before assembling the release
to package the file with the application's private resources:

```sh
rebar3 as swagger compile
escript export_swagger.escript _build/swagger config/sys.config priv/swagger.json
rebar3 release
```

In the installed application's Erlang code, locate that packaged file with:

```erlang
filename:join(code:priv_dir(example), "swagger.json").
```

The exporter is enabled only inside the build script's VM, so running the
release does not repeat the export. If you separately enable a runtime
exporter, choose an explicit writable output location rather than relying
on the release process's working directory or assuming that `priv` is writable.

If the release is built without a bundled ERTS, a compatible Erlang/OTP runtime
must be available in `PATH` on the target machine before using its start
script. To bundle the runtime instead, explicitly configure
`{include_erts, true}` in the `relx` options. This is independent of JSON export
and packaging.
