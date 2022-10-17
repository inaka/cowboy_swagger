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
        , add_definition_test/1
        , add_definition_array_test/1
        , schema_test/1
        , parameters_ref_test/1
        ]).

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
  set_openapi_url("/basepath"),
  Trails = test_trails(),
  SwaggerJson = cowboy_swagger:to_json(Trails),
  Result = jsx:decode(SwaggerJson, [return_maps]),
  #{<<"info">> := #{<<"title">> := <<"Example API">>},
    <<"openapi">> := <<"3.0.0">>,
    <<"servers">> := [#{<<"url">> := <<"/basepath">>}],
    <<"paths">> :=
    #{<<"/a">> :=
    #{<<"get">> :=
    #{<<"description">> := <<"bla bla bla">>,
      <<"parameters">> := [],
      <<"responses">> :=
      #{<<"200">> :=
      #{<<"content">> :=
      #{<<"application/json">> :=
      #{<<"schema">> :=
      #{<<"title">> := <<"bla">>,
        <<"type">> :=
        <<"string">>}}},
        <<"description">> := <<"200 OK">>}}}},
      <<"/a/{b}/{c}">> :=
      #{<<"delete">> :=
      #{<<"description">> := <<"bla bla bla">>,
        <<"parameters">> :=
        [#{<<"description">> := <<"bla">>,
          <<"in">> := <<"path">>, <<"name">> := <<"b">>,
          <<"required">> := false,
          <<"schema">> := #{<<"type">> := <<"string">>}}],
        <<"responses">> := #{}},
        <<"get">> :=
        #{<<"description">> := <<"bla bla bla">>,
          <<"parameters">> :=
          [#{<<"description">> := <<"bla">>,
            <<"in">> := <<"path">>, <<"name">> := <<"b">>,
            <<"required">> := false,
            <<"schema">> := #{<<"type">> := <<"string">>}},
            #{<<"description">> := <<"bla">>,
              <<"in">> := <<"path">>, <<"name">> := <<"c">>,
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
          <<"in">> := <<"path">>, <<"name">> := <<"b">>,
          <<"required">> := false,
          <<"schema">> := #{<<"type">> := <<"string">>}}],
        <<"responses">> := #{}},
        <<"get">> :=
        #{<<"description">> := <<"bla bla bla">>,
          <<"parameters">> :=
          [#{<<"description">> := <<"bla">>,
            <<"in">> := <<"path">>, <<"name">> := <<"b">>,
            <<"required">> := false,
            <<"schema">> := #{<<"type">> := <<"string">>}},
            #{<<"description">> := <<"bla">>,
              <<"in">> := <<"path">>, <<"name">> := <<"c">>,
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
  set_swagger_version(swagger_2_0),
  perform_add_definition_test(),
  perform_add_completed_definition_test(),

  set_swagger_version(openapi_3_0_0),
  perform_add_definition_test(),
  perform_add_completed_definition_test(),

  {comment, ""}.

-spec add_definition_array_test(Config::cowboy_swagger_test_utils:config()) ->
  {comment, string()}.
add_definition_array_test(_Config) ->
  set_swagger_version(swagger_2_0),
  perform_add_definition_array_test(),

  set_swagger_version(openapi_3_0_0),
  perform_add_definition_array_test(),

  {comment, ""}.

-spec schema_test(Config::cowboy_swagger_test_utils:config()) ->
  {comment, string()}.
schema_test(_Config) ->
  set_swagger_version(swagger_2_0),
  #{<<"$ref">> := <<"#/definitions/Pet">>} = cowboy_swagger:schema(<<"Pet">>),

  set_swagger_version(openapi_3_0_0),
  #{<<"$ref">> := <<"#/components/schemas/Pet">>} = cowboy_swagger:schema(<<"Pet">>),

  {comment, ""}.

-spec parameters_ref_test(Config::cowboy_swagger_test_utils:config()) ->
  {comment, string()}.
parameters_ref_test(_Config) ->
  set_swagger_version(openapi_3_0_0),
  cowboy_swagger:add_definition(
    #{<<"page">> =>
      #{description => <<"results per page (max 100)">>, example => 1,
         in => query, name => per_page,
         schema => #{example => 1, maximum => 100, minimum => 1, type => integer}}}),
  {ok, SwaggerSpec1} = application:get_env(cowboy_swagger, global_spec),
  JsonDefinitions = cowboy_swagger:get_existing_definitions(SwaggerSpec1, parameters),
  true = maps:is_key(<<"page">>, JsonDefinitions),
  {comment, ""}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

perform_add_completed_definition_test() ->
  %%
  %% Given
  %%
  ct:comment("Add first definition"),
  Name1 = <<"CostumerDefinition">>,
  Properties1 = test_properties_one(),
  Definition1 =
    #{ Name1 =>
      #{ type => <<"object">>
       , properties => Properties1}
    },

  ct:comment("Add second definition"),
  Name2 = <<"CarDefinition">>,
  Properties2 = test_properties_two(),
  Definition2 =
    #{ Name1 =>
      #{ type => <<"object">>
       , properties => Properties2}
    },
  %%
  %% When
  %%
  ok = cowboy_swagger:add_definition(Definition1),
  ok = cowboy_swagger:add_definition(Definition2),

  %%
  %% Then
  %%
  {ok, SwaggerSpec1} = application:get_env(cowboy_swagger, global_spec),
  JsonDefinitions = cowboy_swagger:get_existing_definitions(SwaggerSpec1, schemas),
  true = maps:is_key(Name1, JsonDefinitions),
  true = maps:is_key(Name2, JsonDefinitions),
  ok.

%% @private
perform_add_definition_test() ->
  %%
  %% Given
  %%
  ct:comment("Add first definition"),
  Name1 = <<"CostumerDefinition">>,
  Properties1 = test_properties_one(),

  ct:comment("Add second definition"),
  Name2 = <<"CarDefinition">>,
  Properties2 = test_properties_two(),

  %%
  %% When
  %%
  ok = cowboy_swagger:add_definition(Name1, Properties1),
  ok = cowboy_swagger:add_definition(Name2, Properties2),

  %%
  %% Then
  %%
  {ok, SwaggerSpec1} = application:get_env(cowboy_swagger, global_spec),
  JsonDefinitions = cowboy_swagger:get_existing_definitions(SwaggerSpec1, schemas),
  true = maps:is_key(Name1, JsonDefinitions),
  true = maps:is_key(Name2, JsonDefinitions),
  ok.

%% @private
perform_add_definition_array_test() ->
  %%
  %% Given
  %%
  ct:comment("Add first definition"),
  Name1 = <<"CostumerDefinition">>,
  Properties1 = test_properties_one(),

  ct:comment("Add second definition"),
  Name2 = <<"CarDefinition">>,
  Properties2 = test_properties_two(),

  %%
  %% When
  %%
  ok = cowboy_swagger:add_definition_array(Name1, Properties1),
  ok = cowboy_swagger:add_definition_array(Name2, Properties2),

  %%
  %% Then
  %%
  {ok, SwaggerSpec1} = application:get_env(cowboy_swagger, global_spec),
  JsonDefinitions = cowboy_swagger:get_existing_definitions(SwaggerSpec1, schemas),
  true = maps:is_key(<<"items">>, maps:get(Name1, JsonDefinitions)),
  true = maps:is_key(<<"items">>, maps:get(Name2, JsonDefinitions)),
  <<"array">> = maps:get(<<"type">>, maps:get(Name1, JsonDefinitions)),
  <<"array">> = maps:get(<<"type">>, maps:get(Name2, JsonDefinitions)),
  ok.

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
      }
    },
  [trails:trail("/a/[:b/[:c/[:d]]]", handler1, [], Metadata),
   trails:trail("/a/:b/[:c]", handler2, [], Metadata),
   trails:trail("/a", handler3, [], Metadata1)|
   cowboy_swagger_handler:trails()].

%% @private
test_properties_one() ->
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
   }.

%% @private
test_properties_two() ->
  #{ <<"brand">> =>
      #{ type => <<"string">>
       , description => <<"Car brand">>
       }
   , <<"year">> =>
      #{ type => <<"string">>
       , description => <<"Production time">>
       , example => <<"1995">>
       }
   }.

%% @private
set_swagger_version(swagger_2_0) ->
  Spec0 = maps:remove(<<"openapi">>, cowboy_swagger:get_global_spec()),
  cowboy_swagger:set_global_spec(Spec0#{swagger => "2.0"});
set_swagger_version(openapi_3_0_0) ->
  Spec0 = maps:remove(<<"swagger">>, cowboy_swagger:get_global_spec()),
  cowboy_swagger:set_global_spec(Spec0#{openapi => "3.0.0"}).

set_openapi_url(Url) ->
  Spec0 = maps:remove(<<"swagger">>, cowboy_swagger:get_global_spec()),
  cowboy_swagger:set_global_spec(Spec0#{openapi => "3.0.0",
                                        servers => [#{url => Url}]}).
