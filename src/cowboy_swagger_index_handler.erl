-module(cowboy_swagger_index_handler).
-moduledoc """
Serves the Swagger UI HTML page with optional search-engine indexing directives.

`cowboy_swagger_handler:trails/0,1` registers this handler at
`<base_path>/index.html`. The page is read from the configured `static_files`
directory. When the `cowboy_swagger` application setting `robots_noindex` is
`true`, a `<meta name="robots" content="noindex, nofollow" />` tag is inserted
before the closing head tag. The setting defaults to `false` and is read for
each request. The source file is never modified.

Custom HTML must contain a closing head tag to receive the directive. HTML
without that tag is returned unchanged. These directives guide search engines;
they do not restrict access to the documentation.
""".

-behaviour(cowboy_rest).

-export([init/2, content_types_provided/2, resource_exists/2, handle_get/2]).
-export_type([state/0]).

-opaque state() :: #{static_files := string(), html => binary()}.

-spec init(cowboy_req:req(), state()) -> {cowboy_rest, cowboy_req:req(), state()}.
init(Req, State) ->
    {cowboy_rest, Req, State}.

-spec content_types_provided(cowboy_req:req(), state()) ->
    {[{binary(), atom()}], cowboy_req:req(), state()}.
content_types_provided(Req, State) ->
    {[{~"text/html", handle_get}], Req, State}.

-spec resource_exists(cowboy_req:req(), state()) -> {boolean(), cowboy_req:req(), state()}.
resource_exists(Req, #{static_files := StaticFiles} = State) ->
    case file:read_file(filename:join(StaticFiles, "index.html")) of
        {ok, HTML} ->
            {true, Req, State#{html => HTML}};
        {error, _Reason} ->
            {false, Req, State}
    end.

-spec handle_get(cowboy_req:req(), state()) -> {binary(), cowboy_req:req(), state()}.
handle_get(Req, #{html := HTML} = State) ->
    Body =
        case application:get_env(cowboy_swagger, robots_noindex, false) of
            true ->
                re:replace(
                    HTML,
                    ~"</head\\s*>",
                    ~"<meta name=\"robots\" content=\"noindex, nofollow\" />\n&",
                    [caseless, {return, binary}]
                );
            false ->
                HTML
        end,
    {Body, Req, State}.
