-module(cowboy_swagger_handler).
-moduledoc """
Cowboy Swagger Handler. This handler exposes a `GET` operation
to enable `/api-docs` to be retrieved and host the Swagger-UI
(located in `priv/swagger` folder).
""".

-behaviour(trails_handler).

-export([trails/0, trails/1]).

-nominal route_match() :: '_' | iodata().
-export_type([route_match/0]).

-doc """
Implements `c:trails_handler:trails/0`. This function returns
trails routes for both: static content (Swagger-UI) and this handler
that returns the `swagger.json`.
""".
-spec trails() -> trails:trails().
trails() ->
    trails(#{}).

-doc """
Implements `c:trails_handler:trails/1`. This function returns
trails routes for both: static content (Swagger-UI) and this handler
that returns the `swagger.json`.
""".
-spec trails(cowboy_swagger_json_handler:options()) -> trails:trails().
trails(Options) ->
    StaticFiles = application:get_env(
        cowboy_swagger, static_files, filename:join(cowboy_swagger_priv(), "swagger")
    ),
    SwaggerPath = application:get_env(cowboy_swagger, base_path, "/api-docs"),
    Redirect =
        trails:trail(
            SwaggerPath,
            cowboy_swagger_redirect_handler,
            {file, StaticFiles ++ "/index.html"},
            #{get => #{hidden => true}}
        ),
    Static =
        trails:trail(
            SwaggerPath ++ "/[...]",
            cowboy_static,
            {dir, StaticFiles, [{mimetypes, cow_mimetypes, all}]},
            #{get => #{hidden => true}}
        ),
    MD = #{get => #{hidden => true}},
    Handler =
        trails:trail(SwaggerPath ++ "/swagger.json", cowboy_swagger_json_handler, Options, MD),
    Initializer = trails:trail(
        SwaggerPath ++ "/swagger-initializer.js",
        cowboy_swagger_initializer_handler,
        #{static_files => StaticFiles, base_path => SwaggerPath},
        MD
    ),
    [Redirect, Handler, Initializer, index_trail(SwaggerPath, StaticFiles), Static].

-doc false.
-spec index_trail(string(), string()) -> trails:trail().
index_trail(SwaggerPath, StaticFiles) ->
    trails:trail(
        SwaggerPath ++ "/index.html",
        cowboy_swagger_index_handler,
        #{static_files => StaticFiles},
        #{get => #{hidden => true}}
    ).

-doc false.
-spec cowboy_swagger_priv() -> string().
cowboy_swagger_priv() ->
    case code:priv_dir(cowboy_swagger) of
        {error, bad_name} ->
            case code:which(cowboy_swagger_handler) of
                cover_compiled ->
                    % required for tests to work
                    "../../priv";
                BeamPath ->
                    filename:join([filename:dirname(BeamPath), "..", "priv"])
            end;
        Path ->
            Path
    end.
