%%% @doc cowboy-swagger main interface.
-module(cowboy_swagger).

%% API
-export([to_json/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Types.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-opaque swagger_parameter() ::
  #{ name        => binary()
   , in          => binary()
   , description => binary()
   , required    => boolean()
   , type        => binary()
   }.
-export_type([swagger_parameter/0]).

-opaque response_obj() ::
  #{ description => binary()
   }.
-type swagger_response() :: #{binary() => response_obj()}.
-export_type([response_obj/0, swagger_response/0]).

%% Swagger map spec
-opaque swagger_map() ::
  #{ description => binary()
   , summary     => binary()
   , parameters  => [swagger_parameter()]
   , tags        => [binary()]
   , consumes    => [binary()]
   , produces    => [binary()]
   , responses   => swagger_response()
   }.
-type metadata() :: trails:metadata(swagger_map()).
-export_type([swagger_map/0, metadata/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Returns the swagger json specification from given `trails'.
-spec to_json(Trails :: [trails:trail()]) -> iolist().
to_json(Trails) ->
  SwaggerSpec = #{paths => swagger_paths(Trails)},
  jiffy:encode(SwaggerSpec, [uescape]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @private
swagger_paths(Trails) ->
  swagger_paths(Trails, #{}).

%% @private
swagger_paths([], Acc) ->
  Acc;
swagger_paths([Trail | T], Acc) ->
  Path = normalize_path(trails:path_match(Trail)),
  Metadata = normalize_map_values(trails:metadata(Trail)),
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
