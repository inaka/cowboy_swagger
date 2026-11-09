<img src="https://www.braveterry.com/wp-content/uploads/2015/03/swagger2.png"/>

# cowboy-swagger

[Swagger](https://swagger.io/) integration for [Cowboy](https://github.com/ninenines/cowboy) (built on [trails](https://github.com/inaka/cowboy-trails)).

![build](https://github.com/inaka/cowboy_swagger/workflows/build/badge.svg)

## Contact Us

If you find any **bugs** or have a **problem** while using this library, please
[open an issue](https://github.com/inaka/cowboy_swagger/issues/new) in this repo
(or a pull request :)).

## Why Cowboy Swagger?

Simple, because there isn't a tool in Erlang to document Cowboy RESTful APIs easy and fast,
and to improve development productivity.

With `cowboy_swagger` is possible to integrate Swagger to your Erlang projects that use Cowboy as a web server.
It is extremely easy to use, and with just a few steps you'll have a nice Web documentation for your RESTful APIs.

To learn a bit more about Swagger, please check this [blog post](https://web.archive.org/web/20161110235900/https://inaka.net/blog/2015/06/23/erlang-swagger-2015/).

## How to Use it?

This is the best part. It is extremely easy.

### 1. Document each Cowboy Handler

Because `cowboy_swagger` runs on top of `trails`, the first thing that you have to do
is document all about your handler within the trails metadata. Keep in mind that
all fields defined within each method into the metadata must be compliant with the
[Swagger specification](https://swagger.io/specification).

For example, suppose that you have `example_echo_handler`, so it must implement the
`c:trails_handler:trails/0` callback:

```erlang
trails() ->
  Metadata =
    #{get =>
      #{tags => [~"echo"],
        description => ~"Gets echo var from the server",
        produces => [~"text/plain"]
      },
      put =>
      #{tags => [~"echo"],
        description => ~"Sets echo var in the server",
        produces => [~"text/plain"],
        parameters => [
          #{name => ~"echo",
            description => ~"Echo message",
            in => ~"path",
            required => false,
            type => ~"string"}
        ]
      }
    },
  [trails:trail("/message/[:echo]", example_echo_handler, [], Metadata)].
```

To get a better idea of how your handler should look like, please check [`example/src/example_echo_handler.erl`](https://github.com/inaka/cowboy_swagger/blob/master/example/src/example_echo_handler.erl).

### 2. Include cowboy_swagger in your app

First, you need to include `cowboy_swagger_handler` module in your list of trails to be compiled.

```erlang
% Include cowboy_swagger_handler in the trails list
Trails = trails:trails([example_echo_handler,
                        example_description_handler,
                        cowboy_swagger_handler]),
% store them
trails:store(Trails),
% and then compile them
Dispatch = trails:single_host_compile(Trails),
```

The snippet of code above is usually placed when you start `cowboy`. Check it [here](https://github.com/inaka/cowboy_swagger/blob/master/example/src/example.erl).

Then add `cowboy_swagger` to the list of apps to be loaded in your `*.app.src` file.

```erlang
{application, example,
 [
  {description, "Cowboy Swagger Basic Example."},
  {vsn, "0.1"},
  {applications,
   [kernel,
    stdlib,
    cowboy,
    trails,
    cowboy_swagger
   ]},
  {modules, []},
  {mod, {example, []}},
  {registered, []},
  {start_phases, [{start_trails_http, []}]}
 ]
}.
```

And that's it, you got it. Now start your application and then you will have access to the API docs
under the path `/api-docs`. Supposing that you're running the app on `localhost:8080`,
that will be [http://localhost:8080/api-docs](http://localhost:8080/api-docs).

## Configuration

Additionally, `cowboy_swagger` can be configured/customized from a `*.config` file:

### app.config

```erlang
[
 %% Other apps ...

 %% cowboy_swagger config
 {cowboy_swagger,
  [
   %% `base_path`: Path where you can access Swagger-UI. Default: `/api-docs`
   {base_path, "/api-docs"},
   %% `robots_noindex`: Add a robots meta tag to the UI HTML. Default: false.
   %% When true, requests noindex, nofollow from search engines.
   {robots_noindex, false},
   %% `swagger_json_url`: URL loaded by Swagger-UI. Defaults to
   %% `base_path` followed by `/swagger.json`. May also be an absolute URL.
   %% Only changes the UI source; the local JSON route stays under `base_path`.
   {swagger_json_url, "/api-docs/swagger.json"},
   %% `json_exporter`: Optional module implementing
   %% cowboy_swagger_json_exporter. No exporter is configured by default.
   %% {json_exporter, my_swagger_exporter},
   %% `static_files`: Static content directory. This is where Swagger-UI
   %% is located. Default: `priv/swagger`.
   %% Remember that Swagger-UI is embedded into `cowboy-swagger` project,
   %% within `priv/swagger` folder. BUT you have to reference that path,
   %% and depending on how you're using `cowboy-swagger` it will be different.
   %% For example, assuming that you want to run your app which has
   %% `cowboy-swagger` as dependency from the console, `static_files` will be:
   {static_files, "./deps/cowboy_swagger/priv/swagger"},

   %% Custom static_files directories must include swagger-initializer.template
   %% with the __SWAGGER_JSON_URL__ placeholder (a complete JavaScript value).

   %% `global_spec`: Global fields for Swagger specification.
   %% If these fields are not set, `cowboy_swagger` will set default values.
   {global_spec,
    #{swagger => ~"2.0",
      info => #{~"title" => ~"Example API"},
      basePath => ~"/api-docs"
     }
   },
   %% `server_spec`:Fields for Swagger specification for a given server.
   %% If these fields are not set for a given server, `cowboy_swagger` will use
   %% `global_spec` for that server.
   {server_spec,
    #{my_server =>
      #{swagger => ~"2.0",
        info => #{~"title" => ~"Example API"},
        basePath => ~"/api-docs"
      },
      my_other_server =>
      #{openapi => ~"3.0.0",
        info => #{~"title" => ~"Other API"},
        basePath => ~"/api-docs"
      }
     }
    }
  ]
 }
].
```

### Exporting generated JSON

Create `src/my_swagger_exporter.erl` in the consuming application to implement
the `cowboy_swagger_json_exporter` behaviour:

```erlang
-module(my_swagger_exporter).
-behaviour(cowboy_swagger_json_exporter).

-export([export/1]).

export(JSON) ->
    file:write_file("swagger.json", JSON).
```

Then select the module in the application's `sys.config` (or `app.config`
loaded by your application), using the same application configuration format
as the [app.config example](#appconfig) above:

```erlang
[
    %% Other apps ...

    %% cowboy_swagger config
    {cowboy_swagger,
        [
            {json_exporter, my_swagger_exporter}
        ]
    }
].
```

No exporter is configured by default. When enabled, the callback receives the
serialized JSON binary synchronously, once per successful generation through
`cowboy_swagger:to_json/1` or `to_json/2`. The functions still return that same
binary. This includes generation triggered by HTTP requests to `swagger.json`;
the setting does not mean “export once” or “export only during builds”.

The callback must return `ok` or `{error, Reason}`. An error return raises
`{json_export_failed, Module, Reason}`; callback exceptions propagate unchanged.
An invalid exporter module or callback also fails generation. Exporters choose
their storage destination and must handle concurrent requests or different
server specifications as needed. Do not call `to_json` from `export/1`, as that
would recursively invoke the exporter.

### Generating swagger.json during a build

Configuring an exporter does not initiate generation. A consuming application
can opt into a post-compile hook that loads its compiled handlers and settings,
collects the trails and calls `cowboy_swagger:to_json/1`. The library does not
install a build hook automatically.

The [example application](example/README.md#build-time-json-export) provides a
complete implementation:

```sh
cd example
rebar3 as swagger compile
```

This writes `example/_build/swagger/swagger.json` without starting the example
application or an HTTP listener. The following build configuration belongs in
`example/rebar.config`; merge it into the existing `profiles` entry if present:

```erlang
{profiles, [
    {swagger, [
        {post_hooks, [
            {compile,
                "escript export_swagger.escript _build/swagger config/sys.config "
                "_build/swagger/swagger.json"}
        ]}
    ]}
]}.
```

The script adds the selected build's dependency and checkout beam directories
to the code path, loads the supplied configuration and invokes generation.
It selects the example exporter only inside its own Erlang VM, leaving the
application's normal runtime configuration unchanged. Export errors cause the
script and the build hook to fail.

For another application, adapt the handler list, exporter and build/configuration
paths. The example script accepts a plain Erlang configuration file containing
one list of application settings. Resolve configuration includes, environment
substitutions or release-specific configuration before invoking it. Definitions
or settings normally registered at application startup must also be prepared
explicitly by the build script. Handlers must expose their trails without
requiring a running application.

### Export paths and releases

`_build/swagger/swagger.json` is the example's build artifact location, not a
path required by the library. Both the build directory and output filename
are explicit script arguments. Relative paths are resolved from the script's
working directory (`example` when using the example's hook). If you customize
the rebar3 build directory or use another profile, update the hook arguments
to match; the script does not discover those paths automatically.

Building a release does not relocate this artifact or include it automatically.
The example's plain `rebar3 release` does not enable the `swagger` profile.
To package the generated file, run the export before assembling the release
and explicitly copy it with a relx overlay, or generate it into the consuming
application's `priv` directory before packaging. See the
[release example](example/README.md#including-the-json-in-a-release).

The example enables its exporter only in the build script's VM. If you also
configure an exporter in a running release, relative output paths resolve
from that process's working directory, which may differ from the project
directory. For runtime writes, configure an explicit writable destination.
For reading a specification packaged in an application's `priv` directory,
resolve its installed location with
`filename:join(code:priv_dir(my_app), "swagger.json")`; the installed application
directory is not necessarily writable.

### Preventing Swagger UI indexing

To request that search engines exclude the documentation page from their index
and do not follow its links, enable `robots_noindex` in your application's
`sys.config` (or the `app.config` loaded by your application):

```erlang
[
    %% Other apps ...

    %% cowboy_swagger config
    {cowboy_swagger,
        [
            {base_path, "/v2/docs"},
            {robots_noindex, true}
        ]
    }
].
```

The HTML returned by `/v2/docs/index.html` will contain the following tag
inside `<head>`:

```html
<meta name="robots" content="noindex, nofollow" />
```

The setting defaults to `false`, which returns the original HTML unchanged.
It is read for each HTML request, so it can also be changed with
`application:set_env(cowboy_swagger, robots_noindex, true)` without restarting
the listener. Refresh the page to fetch the updated HTML.

Custom `static_files` directories are supported: the handler reads their
`index.html` and inserts the tag before the first closing `</head>` tag
(case-insensitive). The file on disk is never modified. HTML without a closing
head tag is returned unchanged. Custom pages should omit an existing robots
meta tag when using this setting, to avoid conflicting directives.

This setting affects the UI HTML page only. It does not add directives to the
JSON specification or restrict access to the documentation. Use authentication
or network restrictions when access must be limited.

### Diferences between global_spec and server_spec

`global_spec` is a single config which contains every specification for your application.

`server_spec` is a config map with servers as key and a specification (just like `global_spec`) as a
value.

Since `global_spec` contains all of the definitions it can create a really messy swagger if your
application starts multiple servers. With `server_spec` everything is separated so your swagger will
stay clean and relevant for a given server.

Note: A server can use either `global_spec` or `server_spec` it cannot be hybrid so you have to make
a choice.

### Definitions

[Definitions](https://swagger.io/specification/#definitionsObject) can be used for describing
[parameters](https://swagger.io/specification/#parametersDefinitionsObject),
[responses](https://swagger.io/specification/#responsesDefinitionsObject) and
[security](https://swagger.io/specification/#securityDefinitionsObject) schemas.

For adding definitions to your app, you have 2 choices:

1. Add a `definitions` key to your cowboy_swagger `global_spec` or `server_spec` map.
2. Add them by calling `cowboy_swagger:add_definition/2` or `cowboy_swagger:add_definition_to_server/3`.

Let's say you want to describe a `POST` call to a `newspapers` endpoint that requires
`name` and `description` fields only, you can do it like this:

**Option 1:**

With `global_spec`

```erlang
[ ... % other configurations
, { cowboy_swagger
  , [ { global_spec
      , #{ swagger => ~"2.0"
         , info => #{~"title" => ~"My app API"}
         , definitions => #{
             ~"RequestBody" =>
               #{ name =>
                   #{ type => ~"string"
                    , description => ~"Newspaper name"
                    }
                , description =>
                    #{ type => ~"string"
                     , description => ~"Newspaper description"
                     }
                }
           }
         }
      }
    ]
  }
]
```

With `server_spec`

```erlang
[ ... % other configurations
, { cowboy_swagger
  , [ { server_spec
      , #{server =>
          #{ swagger => ~"2.0"
           , info => #{~"title" => ~"My app API"}
           , definitions => #{
               ~"RequestBody" =>
                 #{ name =>
                     #{ type => ~"string"
                      , description => ~"Newspaper name"
                      }
                  , description =>
                      #{ type => ~"string"
                       , description => ~"Newspaper description"
                       }
                  }
             }
          }
        }
      }
    ]
  }
]
```

**Option 2:**

For the second choice, you can do it for example in one or several `start_phases`,
directly in your handler or any other place you want.

```erlang
-spec trails() -> trails:trails().
trails() ->
  DefinitionName = ~"RequestBody",
  DefinitionProperties =
    #{ ~"name" =>
         #{ type => ~"string"
          , description => ~"Newspaper name"
          }
     , ~"description" =>
         #{ type => ~"string"
          , description => ~"Newspaper description"
          }
     },
  % Add the definition
  ok = cowboy_swagger:add_definition(DefinitionName, DefinitionProperties),
  ...
```

```erlang
-spec trails() -> trails:trails().
trails() ->
  Server = my_ranch_listener,
  DefinitionName = ~"RequestBody",
  DefinitionProperties =
    #{ ~"name" =>
         #{ type => ~"string"
          , description => ~"Newspaper name"
          }
     , ~"description" =>
         #{ type => ~"string"
          , description => ~"Newspaper description"
          }
     },
  % Add the definition
  ok = cowboy_swagger:add_definition_to_server(Server, DefinitionName, DefinitionProperties),
  ...
```

Now in your handler's trails callback function you can use it:

```erlang
...
  RequestBody =
    #{ name => ~"request body"
     , in => body
     , description => ~"request body (as json)"
     , required => true
       % Use the previously created `RequestBody' definition
     , schema => cowboy_swagger:schema(~"RequestBody")
     },
  Metadata =
    #{ get =>
       #{ tags => [~"newspapers"]
        , description => ~"Returns the list of newspapers"
        , produces => [~"application/json"]
        }
     , post =>
       # { tags => [~"newspapers"]
         , description => ~"Creates a new newspaper"
         , consumes => [~"application/json"]
         , produces => [~"application/json"]
         , parameters => [RequestBody] % and then use that parameter here
         }
     },
  Path = "/newspapers",
  Options = #{path => Path},
  [trails:trail(Path, newspapers_handler, Options, Metadata)].
```

What this does for you is add a nice `response`, `parameter` or `security`
model in swagger-ui, so client developers will know exactly what parameters
the API expects for every endpoint.

## Example

For more information about `cowboy_swagger` and how to use it, please check this [Example](https://github.com/inaka/cowboy_swagger/tree/master/example).
