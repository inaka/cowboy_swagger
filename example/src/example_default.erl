-module(example_default).

-behaviour(cowboy_rest).

-export([init/2, rest_init/2, content_types_accepted/2, content_types_provided/2,
         forbidden/2, resource_exists/2]).

%% cowboy
init(Req, _Opts) ->
    {cowboy_rest, Req, #{}}.

rest_init(Req, _Opts) ->
    {ok, Req, #{}}.

content_types_accepted(Req, State) ->
    {[{'*', handle_put}], Req, State}.

content_types_provided(Req, State) ->
    {[{<<"text/plain">>, handle_get}], Req, State}.

forbidden(Req, State) ->
    {false, Req, State}.

resource_exists(Req, State) ->
    {true, Req, State}.
