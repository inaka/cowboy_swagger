-module(cowboy_swagger_initializer_handler).
-moduledoc false.

-behaviour(cowboy_rest).

-export([init/2, content_types_provided/2, resource_exists/2, handle_get/2]).

-export_type([state/0]).

-opaque state() :: #{static_files := string(), base_path := string()}.

-spec init(cowboy_req:req(), state()) -> {cowboy_rest, cowboy_req:req(), state()}.
init(Req, State) ->
    {cowboy_rest, Req, State}.

-spec content_types_provided(cowboy_req:req(), state()) ->
    {[{binary(), atom()}], cowboy_req:req(), state()}.
content_types_provided(Req, State) ->
    {[{~"application/javascript", handle_get}], Req, State}.

-spec resource_exists(cowboy_req:req(), state()) -> {boolean(), cowboy_req:req(), state()}.
resource_exists(Req, #{static_files := StaticFiles} = State) ->
    {filelib:is_regular(filename:join(StaticFiles, "swagger-initializer.template")), Req, State}.

-spec handle_get(cowboy_req:req(), state()) -> {binary(), cowboy_req:req(), state()}.
handle_get(Req, #{static_files := StaticFiles, base_path := BasePath} = State) ->
    {ok, Template} = file:read_file(filename:join(StaticFiles, "swagger-initializer.template")),
    URL = application:get_env(cowboy_swagger, swagger_json_url, BasePath ++ "/swagger.json"),
    EncodedURL = iolist_to_binary(json:encode(unicode:characters_to_binary(URL))),
    Body = binary:replace(Template, ~"__SWAGGER_JSON_URL__", EncodedURL, [global]),
    {Body, Req, State}.
