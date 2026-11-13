-module(example_echo_handler).

-behaviour(cowboy_rest).

-include_lib("mixer/include/mixer.hrl").

-mixin([
    {example_default, [
        init/2,
        rest_init/2,
        content_types_accepted/2,
        content_types_provided/2,
        resource_exists/2
    ]}
]).

-export([allowed_methods/2, handle_put/2, handle_get/2]).

-behaviour(trails_handler).

-export([trails/0]).

trails() ->
    Response = #{
        ~"200" => #{
            description => ~"Echo message",
            content => #{'text/plain' => #{schema => #{type => string}}}
        }
    },
    Get = #{
        get => #{
            tags => [~"echo"],
            summary => ~"Read the current message",
            responses => Response
        }
    },
    Put = #{
        put => #{
            tags => [~"echo"],
            summary => ~"Update the message",
            parameters => [
                #{
                    name => ~"echo",
                    in => path,
                    required => true,
                    schema => #{type => string, example => ~"Hello, World!"}
                }
            ],
            responses => Response
        }
    },
    [
        trails:trail("/message", ?MODULE, [], Get),
        trails:trail("/message/:echo", ?MODULE, [], Put)
    ].

allowed_methods(Req, State) ->
    {[~"GET", ~"PUT", ~"HEAD"], Req, State}.

handle_get(Req, State) ->
    Echo = application:get_env(example, echo, ""),
    Body = [~"You Get an echo! ", Echo],
    {Body, Req, State}.

handle_put(Req, State) ->
    Echo = cowboy_req:binding(echo, Req, ""),
    application:set_env(example, echo, Echo),
    Body = [~"You put an echo! ", Echo],
    Req1 = cowboy_req:set_resp_body(Body, Req),
    {true, Req1, State}.
