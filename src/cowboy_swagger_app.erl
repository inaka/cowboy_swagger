%%% @hidden
-module(cowboy_swagger_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-spec start(term(), term()) -> {error, term()} | {ok, pid()}.
start(_Type, _Args) ->
  cowboy_swagger_sup:start_link().

-spec stop(term()) -> ok.
stop(_State) ->
  ok.
