-module(cowboy_swagger_SUITE).

-include_lib("mixer/include/mixer.hrl").
-mixin([
        {cowboy_swagger_test_utils,
         [ init_per_suite/1
         , end_per_suite/1
         ]}
       ]).

-export([all/0]).
-export([to_json_test/1]).

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
  Result = jiffy:decode(SwaggerJson, [return_maps]),
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
          ],
          <<"responses">> := #{}
        },
        <<"get">> :=
        #{<<"description">> := <<"bla bla bla">>,
          <<"produces">> := [<<"application/json">>],
          <<"parameters">> := [
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
          ],
          <<"responses">> := #{}
        },
        <<"post">> :=
        #{<<"description">> := <<"bla bla bla">>,
          <<"parameters">> := [
            #{<<"description">> := <<"bla">>,
              <<"in">> := <<"body">>,
              <<"name">> := <<"Request Body">>,
              <<"required">> := true,
              <<"type">> := <<"string">>}
          ],
          <<"responses">> := #{<<"200">> := #{<<"description">> := <<"bla">>}}
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
          ],
          <<"responses">> := #{<<"200">> := #{<<"description">> := <<"bla">>}}
        }
      }
    }
  } = Result,
  #{<<"paths">> := Paths} = Result,
  2 = maps:size(Paths),
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
                 #{description => "bla",
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
                ],
                responses => #{<<"200">> => #{description => "bla"}}
               }
     },
  [trails:trail("/a/[:b/[:c/[:d]]]", handler1, [], Metadata),
    trails:trail("/a/:b/[:c]", handler2, [], Metadata),
    trails:trail("/api-docs", cowboy_swagger_handler, [], Metadata),
    trails:trail("/[...]", cowboy_swagger_handler, [], Metadata)].
