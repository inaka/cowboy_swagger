-module(cowboy_swagger_path_validator).
-moduledoc """
Optional validation of generated path templates against documented parameters.

Enable the `cowboy_swagger` application setting `validate_path_parameters`
to require a matching `in: path` parameter for each template binding in every
visible operation. Operation and path-level parameters are combined. Local
parameter references are resolved against the generated specification.

Missing parameters raise `{missing_path_parameters, Path, Method, Names}`.
If missing names cannot be checked because of a broken, circular or external
reference, validation raises
`{unresolved_path_parameter_reference, Path, Method, Reference}` instead.
References are never fetched from the network. The setting defaults to `false`.
""".

-export([maybe_validate/1, validate/1]).

-spec maybe_validate(map()) -> ok.
maybe_validate(Spec) ->
    case application:get_env(cowboy_swagger, validate_path_parameters, false) of
        false ->
            ok;
        true ->
            validate(Spec)
    end.

-doc false.
-spec validate(map()) -> ok.
validate(Spec) ->
    maps:foreach(
        fun(Path, Item) -> validate_path(Path, Item, Spec) end,
        maps:get(~"paths", Spec, #{})
    ).

-doc false.
-spec validate_path(binary(), map(), map()) -> ok.
validate_path(Path, Item, Spec) ->
    case re:run(Path, ~"\\{([^{}]+)\\}", [global, {capture, [1], binary}]) of
        nomatch ->
            ok;
        {match, Matches} ->
            Bindings = lists:usort([Name || [Name] <:- Matches]),
            Common = maps:get(~"parameters", Item, []),
            maps:foreach(
                fun(Method, Operation) ->
                    validate_operation(
                        Path,
                        Method,
                        Operation,
                        Bindings,
                        Common,
                        Spec
                    )
                end,
                Item
            )
    end.

-doc false.
-spec validate_operation(binary(), binary(), term(), [binary()], [map()], map()) -> ok.
validate_operation(_Path, _Method, Operation, _Bindings, _Common, _Spec) when
    not is_map(Operation)
->
    ok;
validate_operation(_Path, <<"x-", _/binary>>, _Operation, _Bindings, _Common, _Spec) ->
    ok;
validate_operation(_Path, _Method, #{~"hidden" := true}, _Bindings, _Common, _Spec) ->
    ok;
validate_operation(Path, Method, Operation, Bindings, Common, Spec) ->
    Parameters = Common ++ maps:get(~"parameters", Operation, []),
    Resolved = [resolve_parameter(Param, Spec, []) || Param <:- Parameters],
    Names = lists:filtermap(fun parameter_name/1, Resolved),
    check_missing(Path, Method, Bindings -- Names, Resolved).

-doc false.
-spec check_missing(binary(), binary(), [binary()], [map() | {unresolved, binary()}]) -> ok.
check_missing(_Path, _Method, [], _Parameters) ->
    ok;
check_missing(Path, Method, Missing, Parameters) ->
    case
        lists:search(
            fun
                ({unresolved, _}) -> true;
                (_) -> false
            end,
            Parameters
        )
    of
        false ->
            error({missing_path_parameters, Path, Method, Missing});
        {value, {unresolved, Ref}} ->
            error({unresolved_path_parameter_reference, Path, Method, Ref})
    end.

-doc false.
-spec parameter_name(map() | {unresolved, binary()}) -> {true, binary()} | false.
parameter_name(#{~"name" := Name, ~"in" := ~"path"}) ->
    {true, Name};
parameter_name(_Parameter) ->
    false.

-doc false.
-spec resolve_parameter(map(), map(), [binary()]) -> map() | {unresolved, binary()}.
resolve_parameter(#{~"$ref" := Ref}, Spec, Seen) ->
    case lists:member(Ref, Seen) of
        true ->
            {unresolved, Ref};
        false ->
            case resolve_reference(Ref, Spec) of
                {ok, Parameter} -> resolve_parameter(Parameter, Spec, [Ref | Seen]);
                error -> {unresolved, Ref}
            end
    end;
resolve_parameter(Parameter, _Spec, _Seen) ->
    Parameter.

-doc false.
-spec resolve_reference(binary(), map()) -> {ok, map()} | error.
resolve_reference(<<"#/", Pointer/binary>>, Spec) ->
    try uri_string:percent_decode(Pointer) of
        Decoded -> find_pointer(binary:split(Decoded, ~"/", [global]), Spec)
    catch
        throw:{error, _, _} -> error
    end;
resolve_reference(_Reference, _Spec) ->
    error.

-doc false.
-spec find_pointer([binary()], term()) -> {ok, map()} | error.
find_pointer([], Value) when is_map(Value) ->
    {ok, Value};
find_pointer([Token | Rest], Value) when is_map(Value) ->
    Key = binary:replace(binary:replace(Token, ~"~1", ~"/", [global]), ~"~0", ~"~", [global]),
    case maps:find(Key, Value) of
        {ok, Next} -> find_pointer(Rest, Next);
        error -> error
    end;
find_pointer(_Tokens, _Value) ->
    error.
