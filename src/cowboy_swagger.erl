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
  #{ name        => binary() | string()
   , in          => binary() | string()
   , description => binary() | string()
   , required    => boolean()
   , type        => binary() | string()
   }.
-export_type([parameter_obj/0]).

-opaque response_obj() ::
  #{ description => binary()
   }.
-type responses_definitions() :: #{binary() => response_obj()}.
-export_type([response_obj/0, responses_definitions/0]).

%% Swagger map spec
-opaque swagger_map() ::
  #{ description => binary() | string()
   , summary     => binary() | string()
   , parameters  => [parameter_obj()]
   , tags        => [binary() | string()]
   , consumes    => [binary() | string()]
   , produces    => [binary() | string()]
   , responses   => responses_definitions()
   }.
-type metadata() :: trails:metadata(swagger_map()).
-export_type([swagger_map/0, metadata/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Returns the swagger json specification from given `trails'.
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

-spec enc_json(jiffy:json_value()) -> iolist().
enc_json(Json) ->
  jiffy:encode(Json, [uescape]).

-spec dec_json(iodata()) -> jiffy:json_value().
dec_json(Data) ->
  try jiffy:decode(Data, [return_maps])
  catch
    _:{error, _} ->
      throw(bad_json)
  end.

-spec swagger_paths([trails:trail()]) -> map().
swagger_paths(Trails) ->
  swagger_paths(Trails, #{}).

-spec validate_metadata(trails:metadata()) -> trails:metadata().
validate_metadata(Metadata) ->
  validate_swagger_map(maps:keys(Metadata), Metadata).

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
validate_swagger_map([], Acc) ->
  Acc;
validate_swagger_map([K | T], Acc) ->
  case maps:get(K, Acc, undefined) of
    undefined ->
      validate_swagger_map(T, Acc);
    Val ->
      Params = validate_swagger_map_params(maps:get(parameters, Val, []), []),
      Responses = maps:from_list(validate_swagger_map_responses(
        maps:to_list(maps:get(responses, Val, #{})), [])),
      NewVal = Val#{parameters => Params, responses => Responses},
      validate_swagger_map(T, maps:put(K, NewVal, Acc))
  end.

%% @private
validate_swagger_map_params([], Acc) ->
  Acc;
validate_swagger_map_params([Param | T] = Params, Acc) ->
  DefName = binary:list_to_bin(
    [<<"param">>, integer_to_binary(length(Params))]),
  Name = maps:get(name, Param, DefName),
  In = maps:get(in, Param, <<"path">>),
  validate_swagger_map_params(T, [Param#{name => Name, in => In} | Acc]).

%% @private
validate_swagger_map_responses([], Acc) ->
  Acc;
validate_swagger_map_responses([{K, ResObj} | T], Acc) ->
  Desc = maps:get(description, ResObj, <<"">>),
  validate_swagger_map_responses(T, [{K, ResObj#{description => Desc}} | Acc]).
