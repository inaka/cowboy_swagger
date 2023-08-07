-module(example_echo_handler).

-include_lib("mixer/include/mixer.hrl").

-mixin([{example_default,
         [init/3,
          rest_init/2,
          content_types_accepted/2,
          content_types_provided/2,
          resource_exists/2]}]).

-export([allowed_methods/2, handle_put/2, handle_get/2]).

%trails
-behaviour(trails_handler).

-export([trails/0]).

-spec trails() -> [trails:trail()].
trails() ->
    Metadata =
        #{get =>
              #{tags => ["echo"],
                description => "Gets echo var from the server",
                responses =>
                    #{<<"200">> =>
                          #{description => <<"Gets echo var from the server 200 OK">>,
                            content => #{'text/plain' => #{schema => #{type => string}}}}}},
          put =>
              #{tags => ["echo"],
                description => "Sets echo var in the server",
                parameters =>
                    [#{name => <<"echo">>,
                       description => <<"Echo message">>,
                       in => <<"path">>,
                       required => false,
                       schema => #{type => string, example => <<"Hello, World!">>}}],
                responses =>
                    #{<<"200">> =>
                          #{description => <<"Gets echo var from the server 200 OK">>,
                            content => #{'text/plain' => #{schema => #{type => string}}}}}}},
    [trails:trail("/message/[:echo]", example_echo_handler, [], Metadata)].

%% cowboy
-spec allowed_methods(Req, State) -> {[<<_:24, _:_*8>>, ...], Req, State}.
allowed_methods(Req, State) ->
    {[<<"GET">>, <<"PUT">>, <<"HEAD">>], Req, State}.

%% internal
-spec handle_get(Req, State) -> {[binary(), ...], Req, State}.
handle_get(Req, State) ->
    Echo = application:get_env(example, echo, ""),
    Body = [<<"You Get an echo!">>, Echo],
    {Body, Req, State}.

-spec handle_put(cowboy_req:req(), State) -> {true, cowboy_req:req(), State}.
handle_put(Req, State) ->
    {Echo, Req1} = cowboy_req:binding(echo, Req, ""),
    application:set_env(example, echo, Echo),
    Body = [<<"You put an echo! ">>, Echo],
    Req2 = cowboy_req:set_resp_body(Body, Req1),
    {true, Req2, State}.
