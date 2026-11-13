-module(example_auth_handler).
-moduledoc "Bearer authentication for the protected example endpoint.".

-behaviour(cowboy_rest).
-behaviour(trails_handler).

-export([trails/0, init/2, is_authorized/2, content_types_provided/2, handle_get/2]).

-spec trails() -> trails:trails().
trails() ->
    Metadata = #{
        get => #{
            tags => [~"auth"],
            summary => ~"Access a protected resource",
            security => [#{~"bearerAuth" => []}],
            responses => #{
                ~"200" => #{
                    description => ~"Authorized",
                    content => #{
                        'application/json' => #{
                            schema => #{
                                type => object,
                                properties => #{authenticated => #{type => boolean}}
                            }
                        }
                    }
                },
                ~"401" => #{description => ~"Missing or incorrect Bearer token"}
            }
        }
    },
    [trails:trail("/protected", ?MODULE, #{}, Metadata)].

-spec init(cowboy_req:req(), map()) -> {cowboy_rest, cowboy_req:req(), map()}.
init(Req, State) ->
    {cowboy_rest, Req, State}.

-spec is_authorized(cowboy_req:req(), map()) ->
    {true | {false, binary()}, cowboy_req:req(), map()}.
is_authorized(Req, State) ->
    Expected = application:get_env(example, bearer_token, ~"example-token"),
    Authorization =
        try cowboy_req:parse_header(~"authorization", Req) of
            Value -> Value
        catch
            exit:{request_error, _, _} -> undefined
        end,
    case Authorization of
        {bearer, Expected} -> {true, Req, State};
        _ -> {{false, ~"Bearer realm=\"example\""}, Req, State}
    end.

-spec content_types_provided(cowboy_req:req(), map()) ->
    {[{binary(), atom()}], cowboy_req:req(), map()}.
content_types_provided(Req, State) ->
    {[{~"application/json", handle_get}], Req, State}.

-spec handle_get(cowboy_req:req(), map()) -> {iodata(), cowboy_req:req(), map()}.
handle_get(Req, State) ->
    {json:encode(#{authenticated => true}), Req, State}.
