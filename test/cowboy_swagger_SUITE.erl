-module(cowboy_swagger_SUITE).

-include_lib("mixer/include/mixer.hrl").
-mixin([
        {cowboy_swagger_test_utils,
         [ init_per_suite/1
         , end_per_suite/1
         ]}
       ]).

-export([all/0]).
-export([ to_json_test/1
        , add_definition_test/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Common test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec all() -> [atom()].
all() ->
  cowboy_swagger_test_utils:all(?MODULE).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test Cases
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec to_json_test(cowboy_swagger_test_utils:config()) -> {atom(), string()}.
to_json_test(_Config) ->
  Trails = test_trails(),
  SwaggerJson = cowboy_swagger:to_json(Trails),
  Result = jsx:decode(SwaggerJson, [return_maps]),
  #{<<"basePath">> := [],
    <<"info">> := #{<<"title">> := <<"Example API">>},
    <<"openapi">> := <<"3.0.0">>,
    <<"paths">> :=
    #{<<"/a">> :=
    #{<<"get">> :=
    #{<<"description">> := <<"bla bla bla">>,
      <<"parameters">> := [],
      <<"produces">> := [<<"application/json">>],
      <<"responses">> :=
      #{<<"200">> :=
      #{<<"description">> := <<"bla">>}}}},
      <<"/a/{b}/{c}">> :=
      #{<<"delete">> :=
      #{<<"description">> := <<"bla bla bla">>,
        <<"parameters">> :=
        [#{<<"description">> := <<"bla">>,
          <<"in">> := <<"path">>,<<"name">> := <<"b">>,
          <<"required">> := false,
          <<"schema">> := #{<<"type">> := <<"string">>}}],
        <<"responses">> := #{}},
        <<"get">> :=
        #{<<"description">> := <<"bla bla bla">>,
          <<"parameters">> :=
          [#{<<"description">> := <<"bla">>,
            <<"in">> := <<"path">>,<<"name">> := <<"b">>,
            <<"required">> := false,
            <<"schema">> := #{<<"type">> := <<"string">>}},
            #{<<"description">> := <<"bla">>,
              <<"in">> := <<"path">>,<<"name">> := <<"c">>,
              <<"required">> := false,
              <<"schema">> :=
              #{<<"example">> := <<"c">>,
                <<"type">> := <<"string">>}}],
          <<"responses">> :=
          #{<<"200">> :=
          #{<<"content">> :=
          #{<<"application/json">> :=
          #{<<"schema">> :=
          #{<<"title">> := <<"bla">>,
            <<"type">> :=
            <<"string">>}}},
            <<"description">> := <<"200 OK">>}}},
        <<"post">> :=
        #{<<"description">> := <<"bla bla bla">>,
          <<"parameters">> :=
          [#{<<"description">> := <<"bla">>,
            <<"in">> := <<"body">>,
            <<"name">> := <<"Request Body">>,
            <<"required">> := true,
            <<"schema">> := #{<<"type">> := <<"string">>}}],
          <<"responses">> :=
          #{<<"200">> :=
          #{<<"description">> := <<"bla">>}}}},
      <<"/a/{b}/{c}/{d}">> :=
      #{<<"delete">> :=
      #{<<"description">> := <<"bla bla bla">>,
        <<"parameters">> :=
        [#{<<"description">> := <<"bla">>,
          <<"in">> := <<"path">>,<<"name">> := <<"b">>,
          <<"required">> := false,
          <<"schema">> := #{<<"type">> := <<"string">>}}],
        <<"responses">> := #{}},
        <<"get">> :=
        #{<<"description">> := <<"bla bla bla">>,
          <<"parameters">> :=
          [#{<<"description">> := <<"bla">>,
            <<"in">> := <<"path">>,<<"name">> := <<"b">>,
            <<"required">> := false,
            <<"schema">> := #{<<"type">> := <<"string">>}},
            #{<<"description">> := <<"bla">>,
              <<"in">> := <<"path">>,<<"name">> := <<"c">>,
              <<"required">> := false,
              <<"schema">> :=
              #{<<"example">> := <<"c">>,
                <<"type">> := <<"string">>}}],
          <<"responses">> :=
          #{<<"200">> :=
          #{<<"content">> :=
          #{<<"application/json">> :=
          #{<<"schema">> :=
          #{<<"title">> := <<"bla">>,
            <<"type">> :=
            <<"string">>}}},
            <<"description">> := <<"200 OK">>}}},
        <<"post">> :=
        #{<<"description">> := <<"bla bla bla">>,
          <<"parameters">> :=
          [#{<<"description">> := <<"bla">>,
            <<"in">> := <<"body">>,
            <<"name">> := <<"Request Body">>,
            <<"required">> := true,
            <<"schema">> := #{<<"type">> := <<"string">>}}],
          <<"responses">> :=
          #{<<"200">> :=
          #{<<"description">> := <<"bla">>}}}}}} = Result,
  #{<<"paths">> := Paths} = Result,
  3 = maps:size(Paths),
  {comment, ""}.

-spec add_definition_test(Config::cowboy_swagger_test_utils:config()) ->
  {comment, string()}.
add_definition_test(_Config) ->
  ct:comment("Add first definition"),
  Name1 = <<"CostumerDefinition">>,
  Properties1 =
    #{ <<"first_name">> =>
        #{ type => <<"string">>
         , description => <<"User first name">>
         , example => <<"Pepito">>
         }
    , <<"last_name">> =>
        #{ type => <<"string">>
         , description => <<"User last name">>
         , example => <<"Perez">>
         }
    },
  ok = cowboy_swagger:add_definition(Name1, Properties1),

  ct:comment("Add second definition"),
  Name2 = <<"CarDefinition">>,
  Properties2 =
    #{ <<"brand">> =>
        #{ type => <<"string">>
         , description => <<"Car brand">>
         }
    , <<"year">> =>
        #{ type => <<"string">>
         , description => <<"Production time">>
         , example => <<"1995">>
         }
    },
  ok = cowboy_swagger:add_definition(Name2, Properties2),
  {ok, SwaggerSpec1} = application:get_env(cowboy_swagger, global_spec),
  JsonDefinitions = maps:get(definitions, SwaggerSpec1),
  true = maps:is_key(Name1, JsonDefinitions),
  true = maps:is_key(Name2, JsonDefinitions),

  {comment, ""}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @private
test_trails() ->
  Metadata =
    #{get => #{description => <<"bla bla bla">>,
               parameters => [
                 #{name => "b",
                   in => "path",
                   description => "bla",
                   schema => #{
                     type => string
                   },
                   required => false},
                 #{name => "c",
                   in => "path",
                   description => "bla",
                   schema => #{
                     type => string,
                     example => <<"c">>
                   },
                   required => false}
               ],
               responses => #{
                 <<"200">> =>
                   #{description => <<"200 OK">>,
                     content =>
                     #{'application/json' =>
                       #{schema =>
                         #{type => string,
                           title => <<"bla">>
                         }
                       }
                     }
                   }
                 }
              },
      delete => #{description => <<"bla bla bla">>,
                  parameters => [
                    #{name => <<"b">>,
                      in => <<"path">>,
                      description => <<"bla">>,
                      required => false,
                      schema => #{
                        type => string
                      }
                    }
                  ]
                 },
      post => #{description => <<"bla bla bla">>,
                parameters => [
                  #{name => <<"Request Body">>,
                    in => <<"body">>,
                    description => <<"bla">>,
                    required => true,
                    schema => #{
                      type => string
                    }
                  }
                ],
                responses => #{<<"200">> => #{description => "bla"}}
               }
     },
  Metadata1 =
    #{
      get => #{description => <<"bla bla bla">>,
        produces => ["application/json"],
        responses => #{<<"200">> => #{description => "bla"}}
      }
    },
  [trails:trail("/a/[:b/[:c/[:d]]]", handler1, [], Metadata),
   trails:trail("/a/:b/[:c]", handler2, [], Metadata),
   trails:trail("/a", handler3, [], Metadata1)|
   cowboy_swagger_handler:trails()].
