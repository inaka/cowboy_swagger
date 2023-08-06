-module(example_default).

-export([init/3, rest_init/2, content_types_accepted/2, content_types_provided/2,
         forbidden/2, resource_exists/2]).

-hank([unnecessary_function_arguments]).

%% cowboy
-spec init(any(), any(), any()) -> {upgrade, protocol, cowboy_rest}.
init(_Transport, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

-spec rest_init(Req, any()) -> {ok, Req, #{}}.
rest_init(Req, _Opts) ->
    {ok, Req, #{}}.

-spec content_types_accepted(Req, State) -> {[{<<_:80>>, handle_put}, ...], Req, State}.
content_types_accepted(Req, State) ->
    {[{<<"text/plain">>, handle_put}], Req, State}.

-spec content_types_provided(Req, State) -> {[{<<_:80>>, handle_get}, ...], Req, State}.
content_types_provided(Req, State) ->
    {[{<<"text/plain">>, handle_get}], Req, State}.

-spec forbidden(Req, State) -> {false, Req, State}.
forbidden(Req, State) ->
    {false, Req, State}.

-spec resource_exists(Req, State) -> {true, Req, State}.
resource_exists(Req, State) ->
    {true, Req, State}.
