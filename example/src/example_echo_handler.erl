-module(example_echo_handler).

-behaviour(cowboy_rest).

-include_lib("mixer/include/mixer.hrl").

-mixin([{example_default,
         [init/2,
          rest_init/2,
          content_types_accepted/2,
          content_types_provided/2,
          resource_exists/2]}]).

-export([allowed_methods/2, handle_put/2, handle_get/2]).

%trails
-behaviour(trails_handler).

-export([trails/0]).

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
                    [#{name => <<"echo-kebab_case">>,
                       description => <<"Echo message">>,
                       in => <<"path">>,
                       required => false,
                       schema => #{type => string, example => <<"Hello, World!">>}}]}},
    [trails:trail("/message/[:echo-kebab_case]", example_echo_handler, [], Metadata)].

%% cowboy
allowed_methods(Req, State) ->
    {[<<"GET">>, <<"PUT">>, <<"HEAD">>], Req, State}.

%% internal
handle_get(Req, State) ->
    Echo = application:get_env(example, echo, ""),
    Body = [<<"You Get an echo!">>, Echo],
    {Body, Req, State}.

handle_put(Req, State) ->
    Echo = cowboy_req:binding('echo-kebab_case', Req, ""),
    application:set_env(example, echo, Echo),
    Body = [<<"You put an echo! ">>, Echo],
    Req1 = cowboy_req:set_resp_body(Body, Req),
    {true, Req1, State}.
