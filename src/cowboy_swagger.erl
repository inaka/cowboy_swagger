-module(cowboy_swagger).
-moduledoc """
Cowboy-Swagger main interface.
""".

-export([
    to_json/1,
    add_definition/1, add_definition/2,
    add_definition_array/2,
    schema/1
]).
-export([
    get_existing_definitions/2,
    get_global_spec/0, get_global_spec/1,
    set_global_spec/1
]).
-export([
    to_json/2,
    add_definition_to_server/2, add_definition_to_server/3,
    add_definition_array_to_server/3,
    server_schema/2
]).
-export([
    get_server_spec/0, get_server_spec/1, get_server_spec/2,
    set_server_spec/2,
    get_existing_server_definitions/3
]).
-export([enc_json/1, dec_json/1]).
-export([swagger_paths/1, validate_metadata/1]).
-export([filter_cowboy_swagger_handler/1]).

-elvis([{elvis_style, no_throw, disable}]).

-doc """
Parameter definition.
""".
-opaque parameter_obj() ::
    #{
        name => binary(),
        in => binary(),
        description => binary(),
        required => boolean(),
        type => binary(),
        schema => binary()
    }.
-export_type([parameter_obj/0]).

-doc """
Response definition.
""".
-opaque response_obj() :: #{description => binary()}.
-doc """
Map of all possible endpoint responses.
""".
-nominal responses_definitions() :: #{binary() => response_obj()}.
-export_type([response_obj/0, responses_definitions/0]).

-doc """
Parameter definition name.
""".
-nominal parameter_definition_name() :: binary().
-doc """
Property definition.
""".
-nominal property_desc() ::
    #{
        type => binary(),
        description => binary(),
        example => binary(),
        items => property_desc()
    }.
-doc """
Map of all properties.
""".
-nominal property_obj() :: #{binary() => property_desc()}.
-doc """
Map of all single-valued possible endpoint parameters.
""".
-nominal parameters_definitions() ::
    #{
        parameter_definition_name() =>
            #{type => binary(), properties => property_obj(), in => binary(), _ => _}
    }.
-doc """
Map of all array-valued possible endpoint parameters.
""".
-nominal parameters_definition_array() ::
    #{
        parameter_definition_name() =>
            #{type => binary(), items => #{type => binary(), properties => property_obj()}}
    }.
-export_type([
    parameter_definition_name/0,
    property_desc/0,
    property_obj/0,
    parameters_definitions/0,
    parameters_definition_array/0
]).

-doc """
Swagger map spec.
""".
-opaque swagger_map() ::
    #{
        description => binary(),
        summary => binary(),
        parameters => [parameter_obj()],
        tags => [binary()],
        consumes => [binary()],
        produces => [binary()],
        responses => responses_definitions()
    }.
-doc """
Trails metadata that's specific for swagger usage (based on `swagger_map/0`).
""".
-nominal metadata() :: trails:metadata(swagger_map()).
-export_type([swagger_map/0, metadata/0]).

-doc """
Swagger version.
""".
-nominal swagger_version() :: swagger_2_0 | openapi_3_0_0.
-export_type([swagger_version/0]).

-doc """
Returns the swagger json specification from given `trails`.
This function basically takes the metadata from each `t:trails:trail/0`
(which must be compliant with Swagger specification) and builds the
required `swagger.json`.
""".
-doc #{group => "Global Spec API"}.
-spec to_json([trails:trail()]) -> binary().
to_json(Trails) ->
    to_json(undefined, Trails).

-doc """
Returns the swagger json specification from given `trails` and `server_spec`.
This function takes the metadata from each `t:trails:trail/0` and combines it
with the data stored in `server_spec`.
If no data is stored in `server_spec` related to the listener, use `global_spec` instead.
""".
-doc #{group => "Server Spec API"}.
-spec to_json(ranch:ref(), [trails:trail()]) -> binary().
to_json(Server, Trails) ->
    NormalizedSpec =
        case get_server_spec(Server, #{}) of
            Spec when map_size(Spec) =:= 0 ->
                Default = #{info => #{title => ~"API-DOCS"}},
                get_global_spec(Default);
            Spec ->
                Spec
        end,
    SanitizeTrails = filter_cowboy_swagger_handler(Trails),
    SwaggerSpec = create_swagger_spec(NormalizedSpec, SanitizeTrails),
    enc_json(SwaggerSpec).

-doc """
Register an array-valued global definition.
""".
-doc #{group => "Global Spec API"}.
-spec add_definition_array(parameter_definition_name(), property_obj()) -> ok.
add_definition_array(Name, Properties) ->
    DefinitionArray = build_definition_array(Name, Properties),
    add_definition(DefinitionArray).

-doc """
Register a single-valued global definition.
""".
-doc #{group => "Global Spec API"}.
-spec add_definition(parameter_definition_name(), property_obj()) -> ok.
add_definition(Name, Properties) ->
    Definition = build_definition(Name, Properties),
    add_definition(Definition).

-doc """
Register a global definition.
""".
-doc #{group => "Global Spec API"}.
-spec add_definition(parameters_definitions() | parameters_definition_array()) -> ok.
add_definition(Definition) ->
    CurrentSpec = get_global_spec(),
    NormDefinition = normalize_json(Definition),
    Type = definition_type(NormDefinition),
    NewDefinitions =
        maps:merge(get_existing_definitions(CurrentSpec, Type), normalize_json(NormDefinition)),
    NewSpec = prepare_new_global_spec(CurrentSpec, NewDefinitions, Type),
    set_global_spec(NewSpec).

definition_type(Definition) ->
    case maps:values(Definition) of
        [#{~"in" := In}] when In =:= ~"query"; In =:= ~"path"; In =:= ~"header" ->
            ~"parameters";
        _ ->
            ~"schemas"
    end.

-doc """
Returns a schema link to an existing global definition.
e.g. `#{~"$ref" => ~"#/components/schemas/the-def"}`.
""".
-doc #{group => "Global Spec API"}.
-spec schema(DefinitionName :: parameter_definition_name()) -> #{<<_:32>> => <<_:64, _:_*8>>}.
schema(DefinitionName) ->
    Version = swagger_version(),
    get_schema_based_on_version(Version, DefinitionName).

-doc """
Returns a schema link to an existing server definition.
e.g. `#{~"$ref" => ~"#/components/schemas/the-def"}`.
""".
-doc #{group => "Server Spec API"}.
-spec server_schema(ranch:ref(), parameter_definition_name()) -> #{<<_:32>> => <<_:64, _:_*8>>}.
server_schema(Server, DefinitionName) ->
    Version = server_swagger_version(Server),
    get_schema_based_on_version(Version, DefinitionName).

-doc """
Returns a schema link to an existing definition.
e.g. `#{~"$ref" => ~"#/components/schemas/the-def"}`.
""".
-spec get_schema_based_on_version(swagger_version(), parameter_definition_name()) ->
    #{<<_:32>> => <<_:64, _:_*8>>}.
get_schema_based_on_version(swagger_2_0, DefinitionName) ->
    #{~"$ref" => <<"#/definitions/", DefinitionName/binary>>};
get_schema_based_on_version(openapi_3_0_0, DefinitionName) ->
    #{~"$ref" => <<"#/components/schemas/", DefinitionName/binary>>}.

-doc """
Adds a definition to a server.
""".
-doc #{group => "Server Spec API"}.
-spec add_definition_to_server(
    ranch:ref(), parameters_definitions() | parameters_definition_array()
) -> ok.
add_definition_to_server(Server, Definition) ->
    CurrentSpec = get_server_spec(Server),
    NormDefinition = normalize_json(Definition),
    Type = definition_type(NormDefinition),
    NewDefinitions =
        maps:merge(
            get_existing_server_definitions(Server, CurrentSpec, Type),
            normalize_json(NormDefinition)
        ),
    NewSpec = prepare_new_server_spec(Server, CurrentSpec, NewDefinitions, Type),
    set_server_spec(Server, NewSpec).

-doc """
Adds a single-valued definition to a server.
""".
-doc #{group => "Server Spec API"}.
-spec add_definition_to_server(ranch:ref(), parameter_definition_name(), property_obj()) -> ok.
add_definition_to_server(Server, Name, Properties) ->
    Definition = build_definition(Name, Properties),
    add_definition_to_server(Server, Definition).

-doc """
Adds an array-valued definition to a server.
""".
-doc #{group => "Server Spec API"}.
-spec add_definition_array_to_server(ranch:ref(), parameter_definition_name(), property_obj()) ->
    ok.
add_definition_array_to_server(Server, Name, Properties) ->
    DefinitionArray = build_definition_array(Name, Properties),
    add_definition_to_server(Server, DefinitionArray).

-doc false.
-spec enc_json(json:encode_value()) -> binary().
enc_json(Json) ->
    iolist_to_binary(json:encode(Json)).

-doc false.
-spec dec_json(iodata()) -> json:decode_value().
dec_json(Data) ->
    json:decode(iolist_to_binary(Data)).

-doc false.
-spec normalize_json(json:encode_value()) -> json:decode_value().
normalize_json(Json) ->
    %% This way all atom keys and atom values (other than true, false, null)
    %% are turned into binaries, recursively.
    dec_json(enc_json(Json)).

-doc false.
-spec swagger_paths([trails:trail()]) -> map().
swagger_paths(Trails) ->
    swagger_paths(Trails, undefined).

-spec swagger_paths([trails:trail()], binary() | string() | undefined) -> map().
swagger_paths(Trails, BasePath) ->
    Paths = translate_swagger_paths(Trails, #{}),
    refactor_base_path(Paths, BasePath).

-doc false.
-spec validate_metadata(trails:metadata(_)) -> metadata().
validate_metadata(Metadata) ->
    validate_swagger_map(Metadata).

-doc false.
-spec filter_cowboy_swagger_handler([trails:trail()]) -> [trails:trail()].
filter_cowboy_swagger_handler(Trails) ->
    %% Keeps only trails with at least one non-hidden method.
    %% (All the cowboy_swagger_handler methdods are marked as hidden.)
    lists:filter(fun has_a_visible_endpoint/1, Trails).

has_a_visible_endpoint(Trail) ->
    [] =/= [Endpoint || Endpoint := Metadata <:- get_metadata(Trail), is_visible(Metadata)].

-doc false.
is_visible(#{~"hidden" := true}) -> false;
is_visible(#{}) -> true;
is_visible(_Metadata) -> false.

-doc """
Returns all existing definitions for the chosen type (`~"schemas"` or `~"parameters"`).
""".
-doc #{group => "Global Spec API"}.
-spec get_existing_definitions(
    CurrentSpec :: json:decode_value(),
    Type :: binary()
) ->
    parameters_definitions() | parameters_definition_array().
get_existing_definitions(CurrentSpec, Type) ->
    Version = swagger_version(),
    get_existing_definitions(Version, CurrentSpec, Type).

-doc """
Returns an existing definition for the chosen type (`~"schemas"` or `~"properties"`).
""".
-doc #{group => "Server Spec API"}.
-spec get_existing_server_definitions(
    Server :: ranch:ref(),
    CurrentSpec :: json:decode_value(),
    Type :: binary()
) ->
    parameters_definitions() | parameters_definition_array().
get_existing_server_definitions(Server, CurrentSpec, Type) ->
    Version = server_swagger_version(Server),
    get_existing_definitions(Version, CurrentSpec, Type).

-doc """
Returns an existing definition for the chosen type (`~"schemas"` or `~"properties"`).
""".
-spec get_existing_definitions(
    swagger_version(), CurrentSpec :: json:decode_value(), Type :: binary()
) -> parameters_definitions() | parameters_definition_array().
get_existing_definitions(swagger_2_0, CurrentSpec, _Type) ->
    maps:get(~"definitions", CurrentSpec, #{});
get_existing_definitions(openapi_3_0_0, CurrentSpec, Type) ->
    case CurrentSpec of
        #{~"components" := #{Type := Def}} ->
            Def;
        _Other ->
            #{}
    end.

-doc """
Returns the global spec.
""".
-doc #{equiv => get_global_spec(#{})}.
-doc #{group => "Global Spec API"}.
-spec get_global_spec() -> json:decode_value().
get_global_spec() ->
    get_global_spec(#{}).

-doc """
Returns the global spec.
""".
-doc #{group => "Global Spec API"}.
-spec get_global_spec(json:encode_value()) -> json:decode_value().
get_global_spec(Default) ->
    normalize_json(application:get_env(cowboy_swagger, global_spec, Default)).

-doc """
Stores the global spec.
""".
-doc #{group => "Global Spec API"}.
-spec set_global_spec(json:encode_value()) -> ok.
set_global_spec(NewSpec) ->
    application:set_env(cowboy_swagger, global_spec, normalize_json(NewSpec)).

-spec get_metadata(trails:trail()) -> json:decode_value().
get_metadata(Trail) ->
    normalize_json(trails:metadata(Trail)).

-doc """
Returns the spec associated with each server.
""".
-doc #{group => "Server Spec API"}.
-spec get_server_spec() -> #{ranch:ref() := json:decode_value()}.
get_server_spec() ->
    ServerSpec = application:get_env(cowboy_swagger, server_spec, #{}),
    #{Server => normalize_json(Spec) || Server := Spec <:- ServerSpec}.

-doc """
Returns the spec associated with a particular server.
""".
-doc #{equiv => get_server_spec(Server, #{})}.
-doc #{group => "Server Spec API"}.
-spec get_server_spec(ranch:ref()) -> json:decode_value().
get_server_spec(Server) ->
    get_server_spec(Server, #{}).

-doc """
Returns the spec associated with a particular server.
""".
-doc #{group => "Server Spec API"}.
-spec get_server_spec(ranch:ref(), json:decode_value()) -> json:decode_value().
get_server_spec(Server, Default) ->
    ServerSpec = application:get_env(cowboy_swagger, server_spec, #{}),
    normalize_json(maps:get(Server, ServerSpec, Default)).

-doc """
Stores the spec associated with a particular server.
""".
-doc #{group => "Server Spec API"}.
-spec set_server_spec(ranch:ref(), json:encode_value()) -> ok.
set_server_spec(Server, NewSpec) ->
    ServerSpec = application:get_env(cowboy_swagger, server_spec, #{}),
    NormalizedSpec = normalize_json(NewSpec),
    application:set_env(cowboy_swagger, server_spec, ServerSpec#{Server => NormalizedSpec}).

-doc false.
-spec swagger_version() -> swagger_version().
swagger_version() ->
    case get_global_spec() of
        #{~"openapi" := ~"3.0.0"} -> openapi_3_0_0;
        _Other -> swagger_2_0
    end.

-spec server_swagger_version(Server :: ranch:ref()) -> swagger_version().
server_swagger_version(Server) ->
    case get_server_spec(Server) of
        #{~"openapi" := ~"3.0.0"} -> openapi_3_0_0;
        _Other -> swagger_2_0
    end.

-doc false.
translate_swagger_paths([], Acc) ->
    Acc;
translate_swagger_paths([Trail | T], Acc) ->
    Path = normalize_path(trails:path_match(Trail)),
    Metadata = validate_metadata(get_metadata(Trail)),
    translate_swagger_paths(T, Acc#{Path => Metadata}).

-doc false.
refactor_base_path(PathMap, undefined) ->
    PathMap;
refactor_base_path(PathMap, BasePath) when is_list(BasePath) ->
    refactor_base_path(PathMap, list_to_binary(BasePath));
refactor_base_path(PathMap, BasePath) ->
    #{remove_base_path(Path, BasePath) => PathValue || Path := PathValue <:- PathMap}.

-doc false.
-spec remove_base_path(binary(), binary()) -> binary().
remove_base_path(Path, BasePath) ->
    BasePathLength = erlang:byte_size(BasePath),
    MatchLength = BasePathLength + 1,
    case binary:match(Path, <<BasePath/binary, $/>>) of
        {0, MatchLength} ->
            binary:part(Path, BasePathLength, erlang:byte_size(Path) - BasePathLength);
        _ ->
            Path
    end.

-doc false.
normalize_path(Path) ->
    re:replace(
        re:replace(Path, "\\:\\w[\\w-]*", "\\{&\\}", [global]),
        "\\[|\\]|\\:",
        "",
        [{return, binary}, global]
    ).

-doc false.
create_swagger_spec(#{~"swagger" := _Version} = GlobalSpec, SanitizeTrails) ->
    BasePath = maps:get(~"basePath", GlobalSpec, undefined),
    SwaggerPaths = swagger_paths(SanitizeTrails, BasePath),
    GlobalSpec#{~"paths" => SwaggerPaths};
create_swagger_spec(#{~"openapi" := _Version} = GlobalSpec, SanitizeTrails) ->
    BasePath = deconstruct_openapi_url(GlobalSpec),
    SwaggerPaths = swagger_paths(SanitizeTrails, BasePath),
    GlobalSpec#{~"paths" => SwaggerPaths};
create_swagger_spec(GlobalSpec, SanitizeTrails) ->
    create_swagger_spec(GlobalSpec#{~"openapi" => ~"3.0.0"}, SanitizeTrails).

-doc false.
deconstruct_openapi_url(GlobalSpec) ->
    [Server | _] = maps:get(~"servers", GlobalSpec, [#{}]),
    Url = maps:get(~"url", Server, ~""),
    maps:get(path, uri_string:parse(Url)).

-doc false.
validate_swagger_map(Map) when is_map(Map) ->
    %% Note that although per-path entries are usually methods such as
    %% `"get": {...}`, there may also be entries whose values are not maps,
    %% such as path-global `"parameters": [...]'.
    F = fun
        (_K, V) when is_map(V) ->
            Params = validate_swagger_map_params(maps:get(~"parameters", V, [])),
            Responses = validate_swagger_map_responses(maps:get(~"responses", V, #{})),
            V#{~"parameters" => Params, ~"responses" => Responses};
        (_K, V) ->
            V
    end,
    maps:map(F, Map);
validate_swagger_map(Other) ->
    Other.

-doc false.
validate_swagger_map_params(Params) ->
    ValidateParams =
        fun(E) ->
            case maps:get(~"name", E, undefined) of
                undefined ->
                    maps:is_key(~"$ref", E);
                _ ->
                    {true, E#{~"in" => maps:get(~"in", E, ~"path")}}
            end
        end,
    lists:filtermap(ValidateParams, Params).

-doc false.
validate_swagger_map_responses(Responses) ->
    #{K => V#{~"description" => maps:get(~"description", V, ~"")} || K := V <:- Responses}.

-doc false.
-spec build_definition(parameter_definition_name(), property_obj()) -> parameters_definitions().
build_definition(Name, Properties) when is_binary(Name) ->
    #{Name => #{~"type" => ~"object", ~"properties" => Properties}}.

-doc false.
-spec build_definition_array(parameter_definition_name(), property_obj()) ->
    parameters_definition_array().
build_definition_array(Name, Properties) when is_binary(Name) ->
    #{
        Name =>
            #{
                ~"type" => ~"array",
                ~"items" => #{~"type" => ~"object", ~"properties" => Properties}
            }
    }.

-doc false.
-spec prepare_new_global_spec(
    CurrentSpec :: json:decode_value(),
    parameters_definitions() | parameters_definition_array(),
    Type :: binary()
) ->
    NewSpec :: json:decode_value().
prepare_new_global_spec(CurrentSpec, Definitions, Type) ->
    prepare_new_spec(swagger_version(), CurrentSpec, Definitions, Type).

-spec prepare_new_server_spec(
    Server :: ranch:ref(),
    CurrentSpec :: json:decode_value(),
    parameters_definitions() | parameters_definition_array(),
    Type :: binary()
) ->
    NewSpec :: json:decode_value().
prepare_new_server_spec(Server, CurrentSpec, Definitions, Type) ->
    prepare_new_spec(server_swagger_version(Server), CurrentSpec, Definitions, Type).

-spec prepare_new_spec(
    Version :: swagger_version(),
    CurrentSpec :: json:decode_value(),
    parameters_definitions() | parameters_definition_array(),
    Type :: binary()
) ->
    NewSpec :: json:decode_value().
prepare_new_spec(swagger_2_0, CurrentSpec, Definitions, _Type) ->
    CurrentSpec#{~"definitions" => Definitions};
prepare_new_spec(openapi_3_0_0, CurrentSpec, Definitions, Type) ->
    Components = maps:get(~"components", CurrentSpec, #{}),
    CurrentSpec#{~"components" => Components#{Type => Definitions}}.
