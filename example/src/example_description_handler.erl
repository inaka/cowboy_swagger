-module(example_description_handler).

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

-export([allowed_methods/2, handle_get/2]).

-behaviour(trails_handler).

-export([trails/0]).

trails() ->
    Metadata =
        #{
            get =>
                #{
                    tags => [~"example"],
                    description => ~"Describe the example API",
                    responses =>
                        #{
                            ~"200" =>
                                #{
                                    description => ~"Describe the example API 200 OK",
                                    content => #{'text/plain' => #{schema => #{type => string}}}
                                }
                        }
                }
        },
    [trails:trail("/description", example_description_handler, [], Metadata)].

allowed_methods(Req, State) ->
    {[~"GET"], Req, State}.

handle_get(Req, State) ->
    {~"Example API: read and update a message, or authorize to access /protected.", Req, State}.
