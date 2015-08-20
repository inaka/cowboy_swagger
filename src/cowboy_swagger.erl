%%% @doc cowboy-swagger main interface.
-module(cowboy_swagger).

%% API
-export([to_json/1]).

%% Utilities
-export([enc_json/1, dec_json/1]).
-export([swagger_paths/1, validate_metadata/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Types.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-opaque parameter_obj() ::
  #{ name        => iodata()
   , in          => iodata()
   , description => iodata()
   , required    => boolean()
   , type        => iodata()
   }.
-export_type([parameter_obj/0]).

-opaque response_obj() ::
  #{ description => binary()
   }.
-type responses_definitions() :: #{binary() => response_obj()}.
-export_type([response_obj/0, responses_definitions/0]).

%% Swagger map spec
-opaque swagger_map() ::
  #{ description => iodata()
   , summary     => iodata()
   , parameters  => [parameter_obj()]
   , tags        => [iodata()]
   , consumes    => [iodata()]
   , produces    => [iodata()]
   , responses   => responses_definitions()
   }.
-type metadata() :: trails:metadata(swagger_map()).
-export_type([swagger_map/0, metadata/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Returns the swagger json specification from given `trails'.
%%      This function basically takes the metadata from each `trails:trail()'
%%      (which must be compliant with Swagger specification) and builds the
%%      required `swagger.json'.
-spec to_json([trails:trail()]) -> iolist().
to_json(Trails) ->
  Default = #{swagger => <<"2.0">>, info => #{title => <<"API-DOCS">>}},
  GlobalSpec = normalize_map_values(
    application:get_env(cowboy_swagger, global_spec, Default)),
  SwaggerSpec = GlobalSpec#{paths => swagger_paths(Trails)},
  enc_json(SwaggerSpec).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Utilities.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @hidden
-spec enc_json(jiffy:json_value()) -> iolist().
enc_json(Json) ->
  jiffy:encode(Json, [uescape]).

%% @hidden
-spec dec_json(iodata()) -> jiffy:json_value().
dec_json(Data) ->
  try jiffy:decode(Data, [return_maps])
  catch
    _:{error, _} ->
      throw(bad_json)
  end.

%% @hidden
-spec swagger_paths([trails:trail()]) -> map().
swagger_paths(Trails) ->
  swagger_paths(Trails, #{}).

%% @hidden
-spec validate_metadata(trails:metadata()) -> trails:metadata().
validate_metadata(Metadata) ->
  validate_swagger_map(Metadata).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @private
swagger_paths([], Acc) ->
  Acc;
swagger_paths([Trail | T], Acc) ->
  Path = normalize_path(trails:path_match(Trail)),
  Metadata = normalize_map_values(validate_metadata(trails:metadata(Trail))),
  swagger_paths(T, maps:put(Path, Metadata, Acc)).

%% @private
normalize_path(Path) ->
  re:replace(
    re:replace(Path, "\\:\\w+", "\\{&\\}", [global]),
    "\\[|\\]|\\:", "", [{return, binary}, global]).

%% @private
normalize_map_values(Map) when is_map(Map) ->
  normalize_map_values(maps:to_list(Map));
normalize_map_values(Proplist) ->
  F = fun({K, V}, Acc) when is_list(V) ->
        case io_lib:printable_list(V) of
          true  -> maps:put(K, list_to_binary(V), Acc);
          false -> maps:put(K, normalize_list_values(V), Acc)
        end;
      ({K, V}, Acc) when is_map(V) ->
        maps:put(K, normalize_map_values(V), Acc);
      ({K, V}, Acc) ->
        maps:put(K, V, Acc)
      end,
  lists:foldl(F, #{}, Proplist).

%% @private
normalize_list_values(List) ->
  F = fun(V, Acc) when is_list(V) ->
          case io_lib:printable_list(V) of
            true  -> [list_to_binary(V) | Acc];
            false -> [normalize_list_values(V) | Acc]
          end;
      (V, Acc) when is_map(V) ->
        [normalize_map_values(V) | Acc];
      (V, Acc) ->
        [V | Acc]
      end,
  lists:foldl(F, [], List).

%% @private
validate_swagger_map(Map) ->
  F = fun(_K, V) ->
        Params = validate_swagger_map_params(maps:get(parameters, V, [])),
        Responses = validate_swagger_map_responses(maps:get(responses, V, #{})),
        V#{parameters => Params, responses => Responses}
      end,
  maps:map(F, Map).

%% @private
validate_swagger_map_params(Params) ->
  ValidateParams =
    fun(E) ->
      case maps:get(name, E, undefined) of
        undefined -> false;
        _         -> {true, E#{in => maps:get(in, E, <<"path">>)}}
      end
    end,
  lists:filtermap(ValidateParams, Params).

%% @private
validate_swagger_map_responses(Responses) ->
  F = fun(_K, V) -> V#{description => maps:get(description, V, <<"">>)} end,
  maps:map(F, Responses).
