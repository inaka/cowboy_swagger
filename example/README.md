# Cowboy Swagger OpenAPI 3.0.0 Example

To try this example, you need GNU `rebar3` , `git` and `Erlang` in your PATH.

```
  rebar3 release
```

To start the example server in a interactive way do this:
```
  _build/default/rel/example/bin/example console
```

Now, if we point our favorite web browser at [http://localhost:8080/api-docs](http://localhost:8080/api-docs),
we should see the swagger API doc.


## Build-time JSON export

To generate the example's OpenAPI JSON during compilation:

```sh
rebar3 as swagger compile
```

The opt-in `swagger` profile runs a post-compile hook that writes
`_build/swagger/swagger.json`. Normal `rebar3 compile` and `rebar3 release`
do not enable this hook.

`export_swagger.escript` loads the compiled example and dependency modules,
reads `config/sys.config`, collects the echo and description handlers' trails
and calls `cowboy_swagger:to_json/1`. It configures `example_json_exporter`
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

For example, add the following entry to the existing `relx` overlay list in
`rebar.config`, preserving the UI asset overlay:

```erlang
{copy, "_build/swagger/swagger.json", "swagger.json"}
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

The installed application can locate that packaged file with:

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
