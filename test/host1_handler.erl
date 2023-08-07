-module(host1_handler).

-include_lib("mixer/include/mixer.hrl").

-mixin([{example_default,
         [init/3,
          rest_init/2,
          content_types_accepted/2,
          content_types_provided/2,
          resource_exists/2]}]).

-export([allowed_methods/2, handle_get/2]).

%trails
-behaviour(trails_handler).

-export([trails/0]).

-spec trails() -> [trails:trail()].
trails() ->
    Metadata =
        #{get =>
              #{tags => ["whoami"],
                description => "Get hostname",
                responses =>
                    #{<<"200">> =>
                          #{description => <<"Get hostname 200 OK">>,
                            content => #{'text/plain' => #{schema => #{type => string}}}}}}},
    [trails:trail("/whoami", host1_handler, #{}, Metadata)].

%% cowboy
-spec allowed_methods(Req, State) -> {[<<_:24>>, ...], Req, State}.
allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

%% internal
-spec handle_get(cowboy_req:req(), State) -> {<<_:40, _:_*8>>, cowboy_req:req(), State}.
handle_get(Req, State) ->
    Host = cowboy_req:host(Req),
    Body = <<"I am ", Host/binary>>,
    {Body, Req, State}.
