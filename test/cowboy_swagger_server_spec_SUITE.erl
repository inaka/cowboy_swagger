-module(cowboy_swagger_server_spec_SUITE).

-include_lib("mixer/include/mixer.hrl").

-mixin([{cowboy_swagger_test_utils, [init_per_suite/1, end_per_suite/1]}]).

-export([all/0]).
-export([to_json_test/1, add_definition_test/1, add_definition_array_test/1,
         different_definitions_per_server_test/1, different_schema_per_server_test/1]).

-hank([unnecessary_function_arguments]).

-spec all() -> [atom()].
all() ->
    cowboy_swagger_test_utils:all(?MODULE).

to_json_test(_Config) ->
    Server1 = server,
    Server2 = other_server,

    set_swagger_version(Server1, openapi_3_0_0),
    set_swagger_version(Server2, openapi_3_0_0),

    ct:comment("Add definition to SERVER2"),
    Name = <<"User">>,
    Properties = #{<<"id">> => #{type => <<"integer">>, format => <<"int64">>}},
    cowboy_swagger:add_definition_to_server(Server2, Name, Properties),
    Metadata =
        #{get =>
              #{description => <<"Endpoint">>,
                responses =>
                    #{<<"200">> =>
                          #{description => <<"200 OK">>,
                            content =>
                                #{'application/json' =>
                                      #{schema => #{type => string, title => <<"Ok result">>}}}}}}},
    Trails = [trails:trail("/a", handler, [], Metadata) | cowboy_swagger_handler:trails()],

    ct:comment("SERVER1 swagger json should not contain any components"),
    SwaggerJson1 = cowboy_swagger:to_json(Server1, Trails),
    Result1 = jsx:decode(SwaggerJson1, [return_maps]),
    #{<<"openapi">> := <<"3.0.0">>,
      <<"paths">> :=
          #{<<"/a">> :=
                #{<<"get">> :=
                      #{<<"description">> := <<"Endpoint">>,
                        <<"parameters">> := [],
                        <<"responses">> :=
                            #{<<"200">> :=
                                  #{<<"content">> :=
                                        #{<<"application/json">> :=
                                              #{<<"schema">> :=
                                                    #{<<"title">> := <<"Ok result">>,
                                                      <<"type">> := <<"string">>}}},
                                    <<"description">> := <<"200 OK">>}}}}}} =
        Result1,

    ct:comment("SERVER2 swagger json should contain added definitions"),
    SwaggerJson2 = cowboy_swagger:to_json(Server2, Trails),
    Result2 = jsx:decode(SwaggerJson2, [return_maps]),
    #{<<"components">> :=
          #{<<"schemas">> :=
                #{<<"User">> :=
                      #{<<"properties">> :=
                            #{<<"id">> :=
                                  #{<<"format">> := <<"int64">>, <<"type">> := <<"integer">>}},
                        <<"type">> := <<"object">>}}},
      <<"openapi">> := <<"3.0.0">>,
      <<"paths">> :=
          #{<<"/a">> :=
                #{<<"get">> :=
                      #{<<"description">> := <<"Endpoint">>,
                        <<"parameters">> := [],
                        <<"responses">> :=
                            #{<<"200">> :=
                                  #{<<"content">> :=
                                        #{<<"application/json">> :=
                                              #{<<"schema">> :=
                                                    #{<<"title">> := <<"Ok result">>,
                                                      <<"type">> := <<"string">>}}},
                                    <<"description">> := <<"200 OK">>}}}}}} =
        Result2,
    {comment, ""}.

add_definition_test(_Config) ->
    Server = server,

    set_swagger_version(Server, swagger_2_0),
    test_add_definition(Server),
    test_add_completed_definition(Server),

    set_swagger_version(Server, openapi_3_0_0),
    test_add_definition(Server),
    test_add_completed_definition(Server),
    {comment, ""}.

add_definition_array_test(_Config) ->
    Server = server,

    set_swagger_version(Server, swagger_2_0),
    test_add_definition_array(Server),

    set_swagger_version(Server, openapi_3_0_0),
    test_add_definition_array(Server),
    {comment, ""}.

different_definitions_per_server_test(_Config) ->
    Server1 = server,
    Server2 = other_server,

    Name = <<"Pet">>,

    ct:comment("Add definition to SERVER1"),
    Properties1 =
        #{<<"name">> => #{type => <<"string">>, description => <<"Pet name">>},
          <<"birthday">> => #{type => <<"string">>, example => <<"2025">>}},
    ok = cowboy_swagger:add_definition_to_server(Server1, Name, Properties1),

    ct:comment("Add definition to SERVER2 with the same name but different propertes"),
    Properties2 =
        #{<<"name">> => #{type => <<"string">>, description => <<"Pet name">>},
          <<"id">> => #{type => <<"integer">>, format => <<"int64">>}},
    ok = cowboy_swagger:add_definition_to_server(Server2, Name, Properties2),

    {ok, #{Server1 := Server1Spec0, Server2 := Server2Spec0}} =
        application:get_env(cowboy_swagger, server_spec),
    JsonDefinitionsServer1 =
        cowboy_swagger:get_existing_server_definitions(Server1, Server1Spec0, schemas),
    JsonDefinitionsServer2 =
        cowboy_swagger:get_existing_server_definitions(Server2, Server2Spec0, schemas),

    ct:comment("Check if both servers have a given property with different definitions"),
    true = maps:is_key(Name, JsonDefinitionsServer1),
    true = maps:is_key(Name, JsonDefinitionsServer2),
    true = JsonDefinitionsServer1 =/= JsonDefinitionsServer2.

different_schema_per_server_test(_Config) ->
    Server1 = server,
    Server2 = other_server,

    set_swagger_version(Server1, swagger_2_0),
    #{<<"$ref">> := <<"#/definitions/Pet">>} =
        cowboy_swagger:server_schema(Server1, <<"Pet">>),

    set_swagger_version(Server2, openapi_3_0_0),
    #{<<"$ref">> := <<"#/components/schemas/Pet">>} =
        cowboy_swagger:server_schema(Server2, <<"Pet">>),
    {comment, ""}.

test_add_definition(Server) ->
    Name1 = <<"CostumerDefinition">>,
    Properties1 = test_properties_one(),

    Name2 = <<"CarDefinition">>,
    Properties2 = test_properties_two(),

    ok = cowboy_swagger:add_definition_to_server(Server, Name1, Properties1),
    ok = cowboy_swagger:add_definition_to_server(Server, Name2, Properties2),

    {ok, #{Server := SwaggerSpec1}} = application:get_env(cowboy_swagger, server_spec),
    JsonDefinitions =
        cowboy_swagger:get_existing_server_definitions(Server, SwaggerSpec1, schemas),
    true = maps:is_key(Name1, JsonDefinitions),
    true = maps:is_key(Name2, JsonDefinitions),
    ok.

test_add_completed_definition(Server) ->
    Name1 = <<"CostumerDefinition">>,
    Properties1 = test_properties_one(),
    Definition1 = #{Name1 => #{type => <<"object">>, properties => Properties1}},

    Name2 = <<"CarDefinition">>,
    Properties2 = test_properties_two(),
    Definition2 = #{Name1 => #{type => <<"object">>, properties => Properties2}},

    ok = cowboy_swagger:add_definition_to_server(Server, Definition1),
    ok = cowboy_swagger:add_definition_to_server(Server, Definition2),

    {ok, #{Server := SwaggerSpec1}} = application:get_env(cowboy_swagger, server_spec),
    JsonDefinitions =
        cowboy_swagger:get_existing_server_definitions(Server, SwaggerSpec1, schemas),
    true = maps:is_key(Name1, JsonDefinitions),
    true = maps:is_key(Name2, JsonDefinitions),
    ok.

test_add_definition_array(Server) ->
    Name1 = <<"CostumerDefinition">>,
    Properties1 = test_properties_one(),

    Name2 = <<"CarDefinition">>,
    Properties2 = test_properties_two(),

    ok = cowboy_swagger:add_definition_array_to_server(Server, Name1, Properties1),
    ok = cowboy_swagger:add_definition_array_to_server(Server, Name2, Properties2),

    {ok, #{Server := SwaggerSpec1}} = application:get_env(cowboy_swagger, server_spec),
    JsonDefinitions =
        cowboy_swagger:get_existing_server_definitions(Server, SwaggerSpec1, schemas),
    true = maps:is_key(<<"items">>, maps:get(Name1, JsonDefinitions)),
    true = maps:is_key(<<"items">>, maps:get(Name2, JsonDefinitions)),
    <<"array">> = maps:get(<<"type">>, maps:get(Name1, JsonDefinitions)),
    <<"array">> = maps:get(<<"type">>, maps:get(Name2, JsonDefinitions)),
    ok.

test_properties_one() ->
    #{<<"first_name">> =>
          #{type => <<"string">>,
            description => <<"User first name">>,
            example => <<"Pepito">>},
      <<"last_name">> =>
          #{type => <<"string">>,
            description => <<"User last name">>,
            example => <<"Perez">>}}.

%% @private
test_properties_two() ->
    #{<<"brand">> => #{type => <<"string">>, description => <<"Car brand">>},
      <<"year">> =>
          #{type => <<"string">>,
            description => <<"Production time">>,
            example => <<"1995">>}}.

set_swagger_version(Server, swagger_2_0) ->
    Spec0 = maps:remove(<<"openapi">>, cowboy_swagger:get_server_spec(Server)),
    cowboy_swagger:set_server_spec(Server, Spec0#{swagger => "2.0"});
set_swagger_version(Server, openapi_3_0_0) ->
    Spec0 = maps:remove(<<"swagger">>, cowboy_swagger:get_server_spec(Server)),
    cowboy_swagger:set_server_spec(Server, Spec0#{openapi => "3.0.0"}).
