-module(cowboy_swagger_redirect_handler).

%% Cowboy callbacks
-export([ init/3
        , rest_init/2
        , content_types_provided/2
        ]).

%% Handlers
-export([resource_exists/2, previously_existed/2, moved_permanently/2]).

-type state() :: #{}.
-type route_match() :: '_' | iodata().
-type options() :: #{server => ranch:ref(), host => route_match()}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Cowboy Callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @hidden
-spec init({atom(), atom()}, cowboy_req:req(), options()) ->
  {upgrade, protocol, cowboy_rest}.
init(_Transport, _Req, _Opts) ->
  {upgrade, protocol, cowboy_rest}.

%% @hidden
-spec rest_init(cowboy_req:req(), options()) ->
  {ok, cowboy_req:req(), options()}.
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
-spec resource_exists(Req::cowboy_req:req(), State::state()) ->
  {boolean(), cowboy_req:req(), state()}.
resource_exists(Req, State) ->
  {false, Req, State}.

%% @hidden
-spec previously_existed(Req::cowboy_req:req(), State::state())->
  {boolean(), cowboy_req:req(), state()}.
previously_existed(Req, State) ->
  {true, Req, State}.

%% @hidden
-spec moved_permanently(Req::cowboy_req:req(), State::state()) ->
  {{boolean(), string()}, cowboy_req:req(), state()}.
moved_permanently(Req, State) ->
  {{true, "/api-docs/index.html"}, Req, State}.
