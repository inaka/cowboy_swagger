-module(cowboy_swagger_path_validator_SUITE).

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([
    disabled_test/1,
    explicit_validation_test/1,
    matching_test/1,
    provided_methods_test/1,
    missing_test/1,
    shared_test/1,
    references_test/1,
    unresolved_test/1,
    optional_and_hidden_test/1,
    server_and_exporter_test/1
]).

-hank([unnecessary_function_arguments]).

-spec all() -> [atom()].
all() ->
    cowboy_swagger_test_utils:all(?MODULE).

-spec init_per_testcase(atom(), cowboy_swagger_test_utils:config()) ->
    cowboy_swagger_test_utils:config().
init_per_testcase(_Test, Config) ->
    Keys = [
        validate_path_parameters,
        global_spec,
        server_spec,
        json_exporter,
        test_exporter_result
    ],
    Saved = [{Key, application:get_env(cowboy_swagger, Key)} || Key <:- Keys],
    lists:foreach(fun(Key) -> ok = application:unset_env(cowboy_swagger, Key) end, Keys),
    ok = cowboy_swagger:set_global_spec(#{openapi => ~"3.0.0"}),
    [{saved_env, Saved} | Config].

-spec end_per_testcase(atom(), cowboy_swagger_test_utils:config()) ->
    cowboy_swagger_test_utils:config().
end_per_testcase(_Test, Config) ->
    lists:foreach(
        fun
            ({Key, undefined}) -> ok = application:unset_env(cowboy_swagger, Key);
            ({Key, {ok, Value}}) -> ok = application:set_env(cowboy_swagger, Key, Value)
        end,
        proplists:get_value(saved_env, Config)
    ),
    Config.

-spec disabled_test(cowboy_swagger_test_utils:config()) -> ok.
disabled_test(_Config) ->
    Trails = route("/places/:id", #{get => #{parameters => [parameter(~"key", ~"path")]}}),
    JSON = cowboy_swagger:to_json(Trails),
    ok = application:set_env(cowboy_swagger, validate_path_parameters, false),
    JSON = cowboy_swagger:to_json(Trails),
    #{~"paths" := #{~"/places/{id}" := _}} = json:decode(JSON),
    ok.

-spec explicit_validation_test(cowboy_swagger_test_utils:config()) -> ok.
explicit_validation_test(_Config) ->
    Bad = route("/places/:id", #{get => #{}}),
    Missing = missing_id_error(),
    %% Explicit validation works even without the automatic-check setting.
    expect_error(Missing, fun() -> cowboy_swagger:validate_paths(Bad) end),
    ok = application:set_env(cowboy_swagger, validate_path_parameters, false),
    expect_error(Missing, fun() -> cowboy_swagger:validate_paths(validation_server, Bad) end),
    ok = application:set_env(cowboy_swagger, json_exporter, cowboy_swagger_test_exporter),
    Good = route("/places/:id", #{get => #{parameters => [parameter(~"id", ~"path")]}}),
    ok = cowboy_swagger:validate_paths(Good),
    ok = cowboy_swagger:validate_paths(unconfigured_server, Good),
    Referenced = route("/places/:id", #{get => #{parameters => [#{'$ref' => ~"#/parameters/id"}]}}),
    ok = cowboy_swagger:set_global_spec(#{
        swagger => ~"2.0",
        parameters => #{id => parameter(~"other", ~"path")}
    }),
    expect_error(Missing, fun() -> cowboy_swagger:validate_paths(Referenced) end),
    ok = cowboy_swagger:set_server_spec(validation_server, #{
        swagger => ~"2.0",
        parameters => #{id => parameter(~"id", ~"path")}
    }),
    ok = cowboy_swagger:validate_paths(validation_server, Referenced),
    receive
        {exported_json, _} -> ct:fail(validation_invoked_exporter)
    after 0 -> ok
    end,
    ok.

-spec matching_test(cowboy_swagger_test_utils:config()) -> ok.
matching_test(_Config) ->
    ok = enable(),
    Params = [parameter(~"shop_id", ~"path"), parameter(~"product-code", ~"path")],
    Methods = [get, put, post, delete, options, head, patch, trace],
    Metadata = maps:from_list([{Method, #{parameters => Params}} || Method <:- Methods]),
    JSON = cowboy_swagger:to_json(route("/shops/:shop_id/products/:product-code", Metadata)),
    #{~"paths" := #{~"/shops/{shop_id}/products/{product-code}" := _}} = json:decode(JSON),
    %% Existing metadata normalization defaults omitted 'in' to 'path'.
    _ = cowboy_swagger:to_json(
        route("/users/:username", #{
            get => #{
                parameters => [#{name => username}]
            }
        })
    ),
    %% Both spec versions validate paths after removing their API base path.
    lists:foreach(
        fun(Spec) ->
            ok = cowboy_swagger:set_global_spec(Spec),
            _ = cowboy_swagger:to_json(
                route("/api/users/:id", #{
                    get => #{
                        parameters => [parameter(~"id", ~"path")]
                    }
                })
            ),
            expect_error(
                {missing_path_parameters, ~"/users/{id}", ~"get", [~"id"]},
                fun() -> cowboy_swagger:to_json(route("/api/users/:id", #{get => #{}})) end
            )
        end,
        [
            #{swagger => ~"2.0", basePath => ~"/api"},
            #{openapi => ~"3.0.0", servers => [#{url => ~"https://example.org/api"}]}
        ]
    ),
    ok.

-spec provided_methods_test(cowboy_swagger_test_utils:config()) -> ok.
provided_methods_test(_Config) ->
    ok = enable(),
    Params = [parameter(~"id", ~"path")],
    Metadata = #{
        get => #{parameters => Params},
        connect => #{parameters => Params},
        subscribe => #{parameters => Params},
        hidden_custom => #{hidden => true},
        parameters => [],
        summary => ~"Path summary",
        description => ~"Path description",
        servers => [#{url => ~"https://example.org"}],
        '$ref' => ~"#/x-paths/example",
        'x-extra' => #{description => ~"Not an HTTP operation"}
    },
    _ = cowboy_swagger:to_json(route("/places/:id", Metadata)),
    %% A valid standard method must not hide an invalid custom method.
    expect_error(
        {missing_path_parameters, ~"/places/{id}", ~"subscribe", [~"id"]},
        fun() -> cowboy_swagger:to_json(route("/places/:id", Metadata#{subscribe => #{}})) end
    ),
    ok.

-spec missing_test(cowboy_swagger_test_utils:config()) -> ok.
missing_test(_Config) ->
    ok = enable(),
    lists:foreach(
        fun(Params) ->
            expect_error(
                missing_id_error(),
                fun() ->
                    cowboy_swagger:to_json(route("/places/:id", #{get => #{parameters => Params}}))
                end
            )
        end,
        [
            [],
            [parameter(~"key", ~"path")],
            [parameter(~"id", ~"query")],
            [parameter(~"ID", ~"path")]
        ]
    ),
    expect_error(
        {missing_path_parameters, ~"/shops/{shop}/items/{item}", ~"delete", [~"item", ~"shop"]},
        fun() ->
            cowboy_swagger:to_json(
                route("/shops/:shop/items/:item", #{
                    get => #{
                        parameters => [parameter(~"shop", ~"path"), parameter(~"item", ~"path")]
                    },
                    delete => #{}
                })
            )
        end
    ),
    ok.

-spec shared_test(cowboy_swagger_test_utils:config()) -> ok.
shared_test(_Config) ->
    ok = enable(),
    JSON = cowboy_swagger:to_json(
        route("/shops/:shop/items/:item", #{
            parameters => [parameter(~"shop", ~"path")],
            get => #{parameters => [parameter(~"item", ~"path"), parameter(~"shop", ~"query")]},
            put => #{parameters => [parameter(~"item", ~"path")]},
            'x-extra' => #{description => ~"Not an operation"}
        })
    ),
    #{~"paths" := #{~"/shops/{shop}/items/{item}" := _}} = json:decode(JSON),
    ok.

-spec references_test(cowboy_swagger_test_utils:config()) -> ok.
references_test(_Config) ->
    ok = enable(),
    lists:foreach(
        fun({Spec, Ref}) ->
            ok = cowboy_swagger:set_global_spec(Spec),
            _ = cowboy_swagger:to_json(
                route("/users/:user_id", #{
                    parameters => [#{'$ref' => Ref}], get => #{}
                })
            )
        end,
        [
            {
                #{swagger => ~"2.0", parameters => #{user => parameter(~"user_id", ~"path")}},
                ~"#/parameters/user"
            },
            {
                #{
                    openapi => ~"3.0.0",
                    components => #{
                        parameters => #{
                            alias => #{'$ref' => ~"#/components/parameters/a~1b~0c%20d"},
                            ~"a/b~c d" => parameter(~"user_id", ~"path")
                        }
                    }
                },
                ~"#/components/parameters/alias"
            }
        ]
    ),
    ok.

-spec unresolved_test(cowboy_swagger_test_utils:config()) -> ok.
unresolved_test(_Config) ->
    ok = enable(),
    ok = cowboy_swagger:set_global_spec(#{
        openapi => ~"3.0.0",
        components => #{
            parameters => #{
                cycle => #{'$ref' => ~"#/components/parameters/cycle"},
                scalar => 42
            }
        }
    }),
    lists:foreach(
        fun(Ref) ->
            expect_error(
                {unresolved_path_parameter_reference, ~"/users/{name}", ~"get", Ref},
                fun() ->
                    cowboy_swagger:to_json(
                        route("/users/:name", #{
                            get => #{
                                parameters => [#{'$ref' => Ref}]
                            }
                        })
                    )
                end
            )
        end,
        [
            ~"#/components/parameters/missing",
            ~"#/components/parameters/cycle",
            ~"#/components/parameters/scalar",
            ~"#/components/parameters/%ZZ",
            ~"https://example.org/parameters.json#/name"
        ]
    ),
    %% A resolved reference with the wrong parameter name is still a mismatch.
    ok = cowboy_swagger:set_global_spec(#{
        openapi => ~"3.0.0",
        components => #{
            parameters => #{
                wrong => parameter(~"other", ~"path")
            }
        }
    }),
    expect_error(
        {missing_path_parameters, ~"/users/{name}", ~"get", [~"name"]},
        fun() ->
            cowboy_swagger:to_json(
                route("/users/:name", #{
                    get => #{
                        parameters => [
                            #{'$ref' => ~"#/components/parameters/wrong"}
                        ]
                    }
                })
            )
        end
    ),
    %% Unrelated references need not be resolved if all bindings are documented.
    _ = cowboy_swagger:to_json(
        route("/users/:name", #{
            get => #{
                parameters => [
                    parameter(~"name", ~"path"), #{'$ref' => ~"https://example.org/query.json"}
                ]
            }
        })
    ),
    ok.

-spec optional_and_hidden_test(cowboy_swagger_test_utils:config()) -> ok.
optional_and_hidden_test(_Config) ->
    ok = enable(),
    expect_error(
        {missing_path_parameters, ~"/users/{slug}", ~"get", [~"slug"]},
        fun() -> cowboy_swagger:to_json(route("/users/[:slug]", #{get => #{}})) end
    ),
    _ = cowboy_swagger:to_json(
        route("/users/[:slug]", #{
            get => #{parameters => [parameter(~"slug", ~"path")]},
            delete => #{hidden => true}
        })
    ),
    _ = cowboy_swagger:to_json(route("/static/[...]", #{get => #{}})),
    _ = cowboy_swagger:to_json(route("/hidden/:id", #{get => #{hidden => true}})),
    ok.

-spec server_and_exporter_test(cowboy_swagger_test_utils:config()) -> ok.
server_and_exporter_test(_Config) ->
    ok = enable(),
    ok = application:set_env(cowboy_swagger, json_exporter, cowboy_swagger_test_exporter),
    Trails = route("/users/:user", #{get => #{parameters => [#{'$ref' => ~"#/parameters/user"}]}}),
    expect_error(
        {unresolved_path_parameter_reference, ~"/users/{user}", ~"get", ~"#/parameters/user"},
        fun() -> cowboy_swagger:to_json(Trails) end
    ),
    receive
        {exported_json, _} -> ct:fail(invalid_spec_exported)
    after 0 -> ok
    end,
    ok = cowboy_swagger:set_server_spec(validation_server, #{
        swagger => ~"2.0",
        parameters => #{user => parameter(~"user", ~"path")}
    }),
    JSON = cowboy_swagger:to_json(validation_server, Trails),
    receive
        {exported_json, JSON} -> ok
    after 1000 -> ct:fail(valid_spec_not_exported)
    end,
    ok.

-doc false.
-spec enable() -> ok.
enable() -> application:set_env(cowboy_swagger, validate_path_parameters, true).

-doc false.
-spec parameter(binary(), binary()) -> map().
parameter(Name, In) -> #{name => Name, in => In, required => true, schema => #{type => string}}.

-doc false.
-spec route(string(), map()) -> [trails:trail()].
route(Path, Metadata) -> [trails:trail(Path, example_description_handler, [], Metadata)].

-doc false.
-spec expect_error(term(), fun(() -> term())) -> ok.
expect_error(Expected, Fun) ->
    try Fun() of
        _ -> ct:fail({expected_error, Expected})
    catch
        error:Expected -> ok
    end.

-doc false.
-spec missing_id_error() -> term().
missing_id_error() -> {missing_path_parameters, ~"/places/{id}", ~"get", [~"id"]}.
