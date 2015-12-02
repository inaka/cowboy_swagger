%%% @doc Cowboy Swagger Handler. This handler exposes a GET operation
%%%      to enable that  `swagger.json' can be retrieved from embedded
%%%      Swagger-UI (located in `priv/swagger' folder).
-module(cowboy_swagger_handler).

%% Cowboy callbacks
-export([ init/3
        , rest_init/2
        , content_types_provided/2
        ]).

%% Handlers
-export([handle_get/2]).

%% Trails
-behaviour(trails_handler).
-export([trails/0, trails/1]).

-type state() :: #{}.
-type route_match() :: '_' | iodata().
-type options() :: #{server => ranch:ref(), host => route_match()}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Cowboy Callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @hidden
-spec init({atom(), atom()}, cowboy_req:req(), state()) ->
  {upgrade, protocol, cowboy_rest}.
init(_Transport, _Req, _Opts) ->
  {upgrade, protocol, cowboy_rest}.

%% @hidden
-spec rest_init(cowboy_req:req(), state()) ->
  {ok, cowboy_req:req(), state()}.
rest_init(Req, Opts) ->
  {ok, Req, Opts}.

%% @hidden
-spec content_types_provided(cowboy_req:req(), state()) ->
  {[term()], cowboy_req:req(), state()}.
content_types_provided(Req, State) ->
  {[{<<"application/json">>, handle_get}], Req, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Handlers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @hidden
handle_get(Req, State) ->
  Server = maps:get(server, State, '_'),
  HostMatch = maps:get(host, State, '_'),
  Trails = trails:all(Server, HostMatch),
  {cowboy_swagger:to_json(Trails), Req, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Trails
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @hidden
%% @doc Implements `trails_handler:trails/0' callback. This function returns
%%      trails routes for both: static content (Swagger-UI) and this handler
%%      that returns the `swagger.json'.
-spec trails() -> trails:trails().
trails() -> trails(#{}).
-spec trails(Options::options()) -> trails:trails().
trails(Options) ->
  StaticFiles =
    case application:get_env(cowboy_swagger, static_files) of
      {ok, Val} -> Val;
      _         -> filename:join(cowboy_swagger_priv(), "swagger")
    end,
  Static1 = trails:trail(
    "/api-docs",
    cowboy_static,
    {file, StaticFiles ++ "/index.html"},
    #{get => #{tags => ["static-content"], description => "index.html"}}),
  Static2 = trails:trail(
    "/api-docs/[...]",
    cowboy_static,
    {dir, StaticFiles, [{mimetypes, cow_mimetypes, all}]},
    #{get => #{tags => ["static-content"], description => "Static Content"}}),
  MD =
    #{get =>
      #{tags => ["api-docs"],
        description => "Retrives swagger's specification.",
        produces => ["application/json"]
      }
    },
  Handler = trails:trail(
    "/api-docs/swagger.json", cowboy_swagger_handler, Options, MD),
  [Static1, Handler, Static2].

%% @private
-spec cowboy_swagger_priv() -> string().
cowboy_swagger_priv() ->
  case code:priv_dir(cowboy_swagger) of
    {error, bad_name} ->
      filename:join(
        [ filename:dirname(code:which(cowboy_swagger_handler))
        , ".."
        , "priv"
        ]);
    Path -> Path
  end.
