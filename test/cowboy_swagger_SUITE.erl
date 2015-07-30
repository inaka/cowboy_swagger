-module(cowboy_swagger_SUITE).

%% CT
-export([all/0, init_per_suite/1, end_per_suite/1]).

%% Test cases
-export([to_json_test/1]).

-type config() :: [{atom(), term()}].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Common test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec all() -> [atom()].
all() ->
  Exports = ?MODULE:module_info(exports),
  [F || {F, 1} <- Exports, F /= module_info].

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
  Config.

-spec end_per_suite(config()) -> config().
end_per_suite(Config) ->
  Config.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test Cases
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec to_json_test(config()) -> {atom(), string()}.
to_json_test(_Config) ->
  Trails = test_trails(),
  SwaggerJson = cowboy_swagger:to_json(Trails),
  #{<<"paths">> :=
    #{<<"/a/{b}/{c}">> :=
      #{<<"delete">> :=
        #{<<"description">> := <<"bla bla bla">>,
          <<"parameters">> := [
            #{<<"description">> := <<"bla">>,
              <<"in">> := <<"path">>,
              <<"name">> := <<"b">>,
              <<"required">> := false,
              <<"type">> := <<"string">>}
          ]
        },
        <<"get">> :=
        #{<<"description">> := <<"bla bla bla">>,
          <<"produces">> := [<<"application/json">>],
          <<"parameters">> := [
            #{<<"description">> := <<"bla">>,
              <<"in">> := <<"path">>,
              <<"name">> := <<"d">>,
              <<"required">> := false,
              <<"type">> := <<"string">>},
            #{<<"description">> := <<"bla">>,
              <<"in">> := <<"path">>,
              <<"name">> := <<"c">>,
              <<"required">> := false,
              <<"type">> := <<"string">>},
            #{<<"description">> := <<"bla">>,
              <<"in">> := <<"path">>,
              <<"name">> := <<"b">>,
              <<"required">> := false,
              <<"type">> := <<"string">>}
          ]
        },
        <<"post">> :=
        #{<<"description">> := <<"bla bla bla">>,
          <<"parameters">> := [
            #{<<"description">> := <<"bla">>,
              <<"in">> := <<"body">>,
              <<"name">> := <<"Request Body">>,
              <<"required">> := true,
              <<"type">> := <<"string">>}
          ]
        }
      },
      <<"/a/{b}/{c}/{d}">> :=
      #{<<"delete">> :=
        #{<<"description">> := <<"bla bla bla">>,
          <<"parameters">> := [
            #{<<"description">> := <<"bla">>,
              <<"in">> := <<"path">>,
              <<"name">> := <<"b">>,
              <<"required">> := false,
              <<"type">> := <<"string">>}
          ]
        },
        <<"get">> :=
        #{<<"description">> := <<"bla bla bla">>,
          <<"produces">> := [<<"application/json">>],
          <<"parameters">> := [
            #{<<"description">> := <<"bla">>,
              <<"in">> := <<"path">>,
              <<"name">> := <<"d">>,
              <<"required">> := false,
              <<"type">> := <<"string">>},
            #{<<"description">> := <<"bla">>,
              <<"in">> := <<"path">>,
              <<"name">> := <<"c">>,
              <<"required">> := false,
              <<"type">> := <<"string">>},
            #{<<"description">> := <<"bla">>,
              <<"in">> := <<"path">>,
              <<"name">> := <<"b">>,
              <<"required">> := false,
              <<"type">> := <<"string">>}
          ]
        },
        <<"post">> :=
        #{<<"description">> := <<"bla bla bla">>,
          <<"parameters">> := [
            #{<<"description">> := <<"bla">>,
              <<"in">> := <<"body">>,
              <<"name">> := <<"Request Body">>,
              <<"required">> := true,
              <<"type">> := <<"string">>}
          ]
        }
      }
    }
  } = jiffy:decode(SwaggerJson, [return_maps]),
  {comment, ""}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @private
test_trails() ->
  Metadata =
    #{get => #{description => <<"bla bla bla">>,
               produces => ["application/json"],
               parameters => [
                 #{name => "b",
                   in => "path",
                   description => "bla",
                   required => false,
                   type => "string"},
                 #{name => "c",
                   in => "path",
                   description => "bla",
                   required => false,
                   type => "string"},
                 #{name => "d",
                   in => "path",
                   description => "bla",
                   required => false,
                   type => "string"}
               ]
              },
      delete => #{description => <<"bla bla bla">>,
                  parameters => [
                    #{name => <<"b">>,
                      in => <<"path">>,
                      description => <<"bla">>,
                      required => false,
                      type => <<"string">>}
                  ]
                 },
      post => #{description => <<"bla bla bla">>,
                parameters => [
                  #{name => <<"Request Body">>,
                    in => <<"body">>,
                    description => <<"bla">>,
                    required => true,
                    type => <<"string">>}
                ]
               }
     },
  [trails:trail("/a/[:b/[:c/[:d]]]", handler1, [], Metadata),
   trails:trail("/a/:b/[:c]", handler2, [], Metadata),
   trails:trail("/a/[:b]/:c/[:d]", handler3, [], Metadata)].
