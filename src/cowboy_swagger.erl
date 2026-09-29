%%% @doc cowboy-swagger main interface.
-module(cowboy_swagger).

%% Global Spec API
-export([to_json/1, add_definition/1, add_definition/2, add_definition_array/2,
         schema/1]).
%% Server Spec API
-export([to_json/2, add_definition_to_server/2, add_definition_to_server/3,
         add_definition_array_to_server/3, server_schema/2]).
-export([get_server_spec/0, get_server_spec/1, get_server_spec/2, set_server_spec/2,
         get_existing_server_definitions/3]).
%% Utilities
-export([enc_json/1, dec_json/1, normalize_json/1]).
-export([swagger_paths/1, validate_metadata/1]).
-export([filter_cowboy_swagger_handler/1]).
-export([get_existing_definitions/2, get_global_spec/0, get_global_spec/1,
         set_global_spec/1]).

% is_visible is used as a maps:filter/2 predicate, which requires a /2 arity function
-hank([{unnecessary_function_arguments, [{is_visible, 2}]}]).

-elvis([{elvis_style, no_throw, disable}]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Types.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-opaque parameter_obj() ::
    #{name => binary(),
      in => binary(),
      description => binary(),
      required => boolean(),
      type => binary(),
      schema => binary()}.

-export_type([parameter_obj/0]).

-opaque response_obj() :: #{description => binary()}.

-type responses_definitions() :: #{binary() => response_obj()}.

-export_type([response_obj/0, responses_definitions/0]).

-type parameter_definition_name() :: binary().
-type property_desc() ::
    #{type => binary(),
      description => binary(),
      example => binary(),
      items => property_desc()}.
-type property_obj() :: #{binary() => property_desc()}.
-type parameters_definitions() ::
    #{parameter_definition_name() =>
          #{type => binary(),
            properties => property_obj(),
            _ => _}}.
-type parameters_definition_array() ::
    #{parameter_definition_name() =>
          #{type => binary(), items => #{type => binary(), properties => property_obj()}}}.

-export_type([parameter_definition_name/0, property_obj/0, parameters_definitions/0,
              parameters_definition_array/0]).

%% Swagger map spec
-opaque swagger_map() ::
    #{description => binary(),
      summary => binary(),
      parameters => [parameter_obj()],
      tags => [binary()],
      consumes => [binary()],
      produces => [binary()],
      responses => responses_definitions()}.

-type metadata() :: trails:metadata(swagger_map()).

-export_type([swagger_map/0, metadata/0]).

-type swagger_version() :: swagger_2_0 | openapi_3_0_0.

-export_type([swagger_version/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Returns the swagger json specification from given `trails'.
%%      This function basically takes the metadata from each `t:trails:trail()'
%%      (which must be compliant with Swagger specification) and builds the
%%      required `swagger.json'.
-spec to_json([trails:trail()]) -> jsx:json_text().
to_json(Trails) ->
    to_json(undefined, Trails).

%% @doc Returns the swagger json specification from given `trails' and `server_spec`.
%%      This function takes the metadata from each `t:trails:trail()' and combines it
%%      with the data stored in server_spec.
%%      If no data is stored in server_spec related to the listener use global_spec instead.
-spec to_json(ranch:ref(), [trails:trail()]) -> jsx:json_text().
to_json(Server, Trails) ->
    NormalizedSpec =
        case get_server_spec(Server, #{}) of
            Spec when map_size(Spec) == 0 ->
                Default = #{info => #{title => <<"API-DOCS">>}},
                get_global_spec(Default);
            Spec ->
                Spec
        end,
    SanitizeTrails = filter_cowboy_swagger_handler(Trails),
    SwaggerSpec = create_swagger_spec(NormalizedSpec, SanitizeTrails),
    enc_json(SwaggerSpec).

-spec add_definition_array(Name :: parameter_definition_name(),
                           Properties :: property_obj()) ->
                              ok.
add_definition_array(Name, Properties) ->
    DefinitionArray = build_definition_array(Name, Properties),
    add_definition(DefinitionArray).

-spec add_definition(Name :: parameter_definition_name(), Properties :: property_obj()) ->
                        ok.
add_definition(Name, Properties) ->
    Definition = build_definition(Name, Properties),
    add_definition(Definition).

-spec add_definition(Definition ::
                         parameters_definitions() | parameters_definition_array()) ->
                        ok.
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
        [#{<<"in">> := In}] when In =:= <<"query">>; In =:= <<"path">>; In =:= <<"header">> ->
            <<"parameters">>;
        _ ->
            <<"schemas">>
    end.

-spec schema(DefinitionName :: parameter_definition_name()) ->
                #{<<_:32>> => <<_:64, _:_*8>>}.
schema(DefinitionName) ->
    Version = swagger_version(),
    get_schema_based_on_version(Version, DefinitionName).

-spec server_schema(Server :: ranch:ref(),
                    DefinitionName :: parameter_definition_name()) ->
                       #{<<_:32>> => <<_:64, _:_*8>>}.
server_schema(Server, DefinitionName) ->
    Version = server_swagger_version(Server),
    get_schema_based_on_version(Version, DefinitionName).

-spec get_schema_based_on_version(Version :: swagger_version(),
                                  DefinitionName :: parameter_definition_name()) ->
                                     #{<<_:32>> => <<_:64, _:_*8>>}.
get_schema_based_on_version(swagger_2_0, DefinitionName) ->
    #{<<"$ref">> => <<"#/definitions/", DefinitionName/binary>>};
get_schema_based_on_version(openapi_3_0_0, DefinitionName) ->
    #{<<"$ref">> => <<"#/components/schemas/", DefinitionName/binary>>}.

-spec add_definition_to_server(Server :: ranch:ref(),
                               Definition ::
                                   parameters_definitions() | parameters_definition_array()) ->
                                  ok.
add_definition_to_server(Server, Definition) ->
    CurrentSpec = get_server_spec(Server),
    NormDefinition = normalize_json(Definition),
    Type = definition_type(NormDefinition),
    NewDefinitions =
        maps:merge(get_existing_server_definitions(Server, CurrentSpec, Type),
                   normalize_json(NormDefinition)),
    NewSpec = prepare_new_server_spec(Server, CurrentSpec, NewDefinitions, Type),
    set_server_spec(Server, NewSpec).

-spec add_definition_to_server(Server :: ranch:ref(),
                               Name :: parameter_definition_name(),
                               Properties :: property_obj()) ->
                                  ok.
add_definition_to_server(Server, Name, Properties) ->
    Definition = build_definition(Name, Properties),
    add_definition_to_server(Server, Definition).

-spec add_definition_array_to_server(Server :: ranch:ref(),
                                     Name :: parameter_definition_name(),
                                     Properties :: property_obj()) ->
                                        ok.
add_definition_array_to_server(Server, Name, Properties) ->
    DefinitionArray = build_definition_array(Name, Properties),
    add_definition_to_server(Server, DefinitionArray).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Utilities.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @private
-spec enc_json(jsx:json_term()) -> jsx:json_text().
enc_json(Json) ->
    jsx:encode(Json, [uescape]).

%% @private
-spec dec_json(iodata()) -> jsx:json_term().
dec_json(Data) ->
    try
        jsx:decode(Data, [return_maps])
    catch
        _:{error, _} ->
            throw(bad_json)
    end.

%% We assume the jsx representation of JSON as Erlang terms:
%%   true/false/null: 'true' | 'false' | 'null'
%%   number:          integer() | float()
%%   string:          binary() | atom()
%%   array:           [ JSON ]
%%   object:          #{ Label => JSON, ... } |  [{ Label, JSON }] | [{}]
%%   date string:     {{Year, Month, Day}, {Hour, Min, Sec}}
%% where
%%   Label:           binary() | atom() | integer()
%%
%% We also detect lists of printable characters (plain Erlang strings) and
%% convert them into binaries. This use is deprecated and should be
%% removed (for example, a json array [64] becomes <<"@">>).
%%
%% When normalizing, we make all strings and labels be binaries,
%% and all objects be maps, not proplists.

%% @private
-spec normalize_json(jsx:json_term()) -> jsx:json_term().
normalize_json(Json) when is_map(Json) ->
    normalize_json_proplist(maps:to_list(Json));
normalize_json([]) ->
    []; % empty array
normalize_json([{}]) ->
    #{}; % special case in jsx for empty map as list
normalize_json([{_K, _V} | _] = Json) ->
    normalize_json_proplist(Json); % map as proplist
normalize_json(Json) when is_list(Json) ->
    case io_lib:printable_list(Json) of
        true ->
            unicode:characters_to_binary(Json);
        false ->
            normalize_json_list(Json)
    end;
normalize_json(true) ->
    true;
normalize_json(false) ->
    false;
normalize_json(null) ->
    null;
normalize_json(Json) when is_atom(Json) ->
    erlang:atom_to_binary(Json, utf8);
normalize_json(Json) ->
    Json.

normalize_json_key(K) when is_atom(K) ->
    erlang:atom_to_binary(K, utf8);
normalize_json_key(K) when is_integer(K) ->
    erlang:integer_to_binary(K);
normalize_json_key(K) ->
    K.

normalize_json_proplist(Proplist) ->
    F = fun({K, V}, Acc) -> maps:put(normalize_json_key(K), normalize_json(V), Acc) end,
    lists:foldl(F, #{}, Proplist).

normalize_json_list(List) ->
    F = fun(V, Acc) -> [normalize_json(V) | Acc] end,
    lists:foldr(F, [], List).

%% @private
-spec swagger_paths([trails:trail()]) -> map().
swagger_paths(Trails) ->
    swagger_paths(Trails, undefined).

-spec swagger_paths([trails:trail()], binary() | string() | undefined) -> map().
swagger_paths(Trails, BasePath) ->
    Paths = translate_swagger_paths(Trails, #{}),
    refactor_base_path(Paths, BasePath).

%% @private
-spec validate_metadata(trails:metadata(_)) -> metadata().
validate_metadata(Metadata) ->
    validate_swagger_map(Metadata).

%% @private
-spec filter_cowboy_swagger_handler([trails:trail()]) -> [trails:trail()].
filter_cowboy_swagger_handler(Trails) ->
    %% Keeps only trails with at least one non-hidden method.
    %% (All the cowboy_swagger_handler methdods are marked as hidden.)
    F = fun(Trail) ->
           MD = get_metadata(Trail),
           maps:size(
               maps:filter(fun is_visible/2, MD))
           /= 0
        end,
    lists:filter(F, Trails).

-spec get_existing_definitions(CurrentSpec :: jsx:json_term(),
                               Type :: atom() | binary()) ->
                                  Definition ::
                                      parameters_definitions() | parameters_definition_array().
get_existing_definitions(CurrentSpec, Type) ->
    Version = swagger_version(),
    get_existing_definitions(Version, CurrentSpec, Type).

-spec get_existing_server_definitions(Server :: ranch:ref(),
                                      CurrentSpec :: jsx:json_term(),
                                      Type :: atom() | binary()) ->
                                         Definition ::
                                             parameters_definitions() |
                                             parameters_definition_array().
get_existing_server_definitions(Server, CurrentSpec, Type) ->
    Version = server_swagger_version(Server),
    get_existing_definitions(Version, CurrentSpec, Type).

-spec get_existing_definitions(Version :: swagger_version(),
                               CurrentSpec :: jsx:json_term(),
                               Type :: atom() | binary()) ->
                                  Definition ::
                                      parameters_definitions() | parameters_definition_array().
get_existing_definitions(Version, CurrentSpec, Type) when is_atom(Type) ->
    get_existing_definitions(Version, CurrentSpec, atom_to_binary(Type, utf8));
get_existing_definitions(swagger_2_0, CurrentSpec, _Type) ->
    maps:get(<<"definitions">>, CurrentSpec, #{});
get_existing_definitions(openapi_3_0_0, CurrentSpec, Type) ->
    case CurrentSpec of
        #{<<"components">> := #{Type := Def}} ->
            Def;
        _Other ->
            #{}
    end.

-spec get_global_spec() -> jsx:json_term().
get_global_spec() ->
    get_global_spec(#{}).

-spec get_global_spec(jsx:json_term()) -> jsx:json_term().
get_global_spec(Default) ->
    normalize_json(application:get_env(cowboy_swagger, global_spec, Default)).

-spec set_global_spec(jsx:json_term()) -> ok.
set_global_spec(NewSpec) ->
    application:set_env(cowboy_swagger, global_spec, normalize_json(NewSpec)).

-spec get_metadata(trails:trail()) -> jsx:json_term().
get_metadata(Trail) ->
    normalize_json(trails:metadata(Trail)).

-spec get_server_spec() -> #{ranch:ref() := jsx:json_term()}.
get_server_spec() ->
    ServerSpec = application:get_env(cowboy_swagger, server_spec, #{}),
    maps:map(fun(_Server, Spec) -> normalize_json(Spec) end, ServerSpec).

-spec get_server_spec(ranch:ref()) -> jsx:json_term().
get_server_spec(Server) ->
    get_server_spec(Server, #{}).

-spec get_server_spec(ranch:ref(), jsx:json_term()) -> jsx:json_term().
get_server_spec(Server, Default) ->
    ServerSpec = application:get_env(cowboy_swagger, server_spec, #{}),
    normalize_json(maps:get(Server, ServerSpec, Default)).

-spec set_server_spec(ranch:ref(), jsx:json_term()) -> ok.
set_server_spec(Server, NewSpec) ->
    ServerSpec = application:get_env(cowboy_swagger, server_spec, #{}),
    NormalizedSpec = normalize_json(NewSpec),
    application:set_env(cowboy_swagger, server_spec, ServerSpec#{Server => NormalizedSpec}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @private
-spec swagger_version() -> swagger_version().
swagger_version() ->
    case get_global_spec() of
        #{<<"openapi">> := <<"3.0.0">>} ->
            openapi_3_0_0;
        #{<<"swagger">> := <<"2.0">>} ->
            swagger_2_0;
        _Other ->
            swagger_2_0
    end.

-spec server_swagger_version(Server :: ranch:ref()) -> swagger_version().
server_swagger_version(Server) ->
    case get_server_spec(Server) of
        #{<<"openapi">> := <<"3.0.0">>} ->
            openapi_3_0_0;
        #{<<"swagger">> := <<"2.0">>} ->
            swagger_2_0;
        _Other ->
            swagger_2_0
    end.

%% @private
is_visible(_Key, Metadata) when is_map(Metadata) ->
    %% Note that `"hidden"` is not a standard flag in OpenAPI
    not maps:get(<<"hidden">>, Metadata, false);
is_visible(_Key, _Metadata) ->
    false.

%% @private
translate_swagger_paths([], Acc) ->
    Acc;
translate_swagger_paths([Trail | T], Acc) ->
    Path = normalize_path(trails:path_match(Trail)),
    Metadata = validate_metadata(get_metadata(Trail)),
    translate_swagger_paths(T, maps:put(Path, Metadata, Acc)).

%% @private
refactor_base_path(PathMap, undefined) ->
    PathMap;
refactor_base_path(PathMap, BasePath) when is_list(BasePath) ->
    refactor_base_path(PathMap, list_to_binary(BasePath));
refactor_base_path(PathMap, BasePath) ->
    Fun = fun(Path, NextPathMap) ->
             maps:put(remove_base_path(Path, BasePath), maps:get(Path, PathMap), NextPathMap)
          end,
    lists:foldl(Fun, #{}, maps:keys(PathMap)).

%% /base_path/api -> /api
%% @private
-spec remove_base_path(binary(), binary()) -> binary().
remove_base_path(Path, BasePath) ->
    BasePathLength = erlang:size(BasePath),
    MatchLength = BasePathLength + 1,
    case binary:match(Path, <<BasePath/binary, "/">>) of
        {0, MatchLength} ->
            binary:part(Path, BasePathLength, erlang:size(Path) - BasePathLength);
        _ ->
            Path
    end.

%% @private
normalize_path(Path) ->
    re:replace(
        re:replace(Path, "\\:\\w[\\w-]*", "\\{&\\}", [global]),
        "\\[|\\]|\\:",
        "",
        [{return, binary}, global]).

%% @private
create_swagger_spec(#{<<"swagger">> := _Version} = GlobalSpec, SanitizeTrails) ->
    BasePath = maps:get(<<"basePath">>, GlobalSpec, undefined),
    SwaggerPaths = swagger_paths(SanitizeTrails, BasePath),
    GlobalSpec#{<<"paths">> => SwaggerPaths};
create_swagger_spec(#{<<"openapi">> := _Version} = GlobalSpec, SanitizeTrails) ->
    BasePath = deconstruct_openapi_url(GlobalSpec),
    SwaggerPaths = swagger_paths(SanitizeTrails, BasePath),
    GlobalSpec#{<<"paths">> => SwaggerPaths};
create_swagger_spec(GlobalSpec, SanitizeTrails) ->
    create_swagger_spec(GlobalSpec#{<<"openapi">> => <<"3.0.0">>}, SanitizeTrails).

%% @private
deconstruct_openapi_url(GlobalSpec) ->
    [Server | _] = maps:get(<<"servers">>, GlobalSpec, [#{}]),
    Url = maps:get(<<"url">>, Server, <<"">>),
    maps:get(path, uri_string:parse(Url)).

%% @private
validate_swagger_map(Map) when is_map(Map) ->
    %% Note that although per-path entries are usually methods such as
    %% `"get": {...}`, there may also be entries whose values are not maps,
    %% such as path-global `"parameters": [...]'.
    F = fun (_K, V) when is_map(V) ->
                Params = validate_swagger_map_params(maps:get(<<"parameters">>, V, [])),
                Responses = validate_swagger_map_responses(maps:get(<<"responses">>, V, #{})),
                V#{<<"parameters">> => Params, <<"responses">> => Responses};
            (_K, V) ->
                V
        end,
    maps:map(F, Map);
validate_swagger_map(Other) ->
    Other.

%% @private
validate_swagger_map_params(Params) ->
    ValidateParams =
        fun(E) ->
           case maps:get(<<"name">>, E, undefined) of
               undefined ->
                   maps:is_key(<<"$ref">>, E);
               _ ->
                   {true, E#{<<"in">> => maps:get(<<"in">>, E, <<"path">>)}}
           end
        end,
    lists:filtermap(ValidateParams, Params).

%% @private
validate_swagger_map_responses(Responses) ->
    F = fun(_K, V) -> V#{<<"description">> => maps:get(<<"description">>, V, <<"">>)} end,
    maps:map(F, Responses).

%% @private
-spec build_definition(Name :: parameter_definition_name(),
                       Properties :: property_obj()) ->
                          parameters_definitions().
build_definition(Name, Properties) when is_atom(Name) ->
    build_definition(erlang:atom_to_binary(Name, utf8), Properties);
build_definition(Name, Properties) when is_binary(Name) ->
    #{Name => #{<<"type">> => <<"object">>, <<"properties">> => Properties}}.

%% @private
-spec build_definition_array(Name :: parameter_definition_name(),
                             Properties :: property_obj()) ->
                                parameters_definition_array().
build_definition_array(Name, Properties) when is_atom(Name) ->
    build_definition_array(erlang:atom_to_binary(Name, utf8), Properties);
build_definition_array(Name, Properties) when is_binary(Name) ->
    #{Name =>
          #{<<"type">> => <<"array">>,
            <<"items">> => #{<<"type">> => <<"object">>, <<"properties">> => Properties}}}.

%% @private
-spec prepare_new_global_spec(CurrentSpec :: jsx:json_term(),
                              Definitions ::
                                  parameters_definitions() | parameters_definition_array(),
                              Type :: binary()) ->
                                 NewSpec :: jsx:json_term().
prepare_new_global_spec(CurrentSpec, Definitions, Type) ->
    prepare_new_spec(swagger_version(), CurrentSpec, Definitions, Type).

-spec prepare_new_server_spec(Server :: ranch:ref(),
                              CurrentSpec :: jsx:json_term(),
                              Definitions ::
                                  parameters_definitions() | parameters_definition_array(),
                              Type :: binary()) ->
                                 NewSpec :: jsx:json_term().
prepare_new_server_spec(Server, CurrentSpec, Definitions, Type) ->
    prepare_new_spec(server_swagger_version(Server), CurrentSpec, Definitions, Type).

-spec prepare_new_spec(Version :: swagger_version(),
                       CurrentSpec :: jsx:json_term(),
                       Definitions :: parameters_definitions() | parameters_definition_array(),
                       Type :: binary()) ->
                          NewSpec :: jsx:json_term().
prepare_new_spec(swagger_2_0, CurrentSpec, Definitions, _Type) ->
    CurrentSpec#{<<"definitions">> => Definitions};
prepare_new_spec(openapi_3_0_0, CurrentSpec, Definitions, Type) ->
    Components = maps:get(<<"components">>, CurrentSpec, #{}),
    CurrentSpec#{<<"components">> => Components#{Type => Definitions}}.
