%%% @doc cowboy-swagger main interface.
-module(cowboy_swagger).

%% API
-export([to_json/1]).

-spec to_json(Trails :: [trails:trail()]) -> binary().
to_json(_Trails) ->
  %% @todo implement this function
  <<>>.