-module(klsn_rule).

%% public functions
-export([
        validate/2
      , normalize/2
    ]).

%% builtin rules
-export([
        any_rule/2
      , boolean_rule/2
      , integer_rule/2
      , float_rule/2
      , number_rule/2
      , timeout_rule/2
      , binstr_rule/2
      , atom_rule/2
      , enum_rule/2
      , optnl_rule/2
      , nullable_integer_rule/2
      , nullable_float_rule/2
      , nullable_number_rule/2
      , nullable_binstr_rule/2
    ]).

-export_type([
        name/0
      , input/0
      , output/0
      , custom/0
      , acc/0
      , rule/0
      , reason/0
      , result/0
    ]).

-type name() :: atom().

-type input() :: term().

-type output() :: term().

-type custom() :: fun( (input(), acc()) -> result() ).

-type acc() :: term().

-type rule() :: {custom, name(), custom(), acc()}
              | any
              | boolean
              | integer
              | float
              | number
              | timeout
              | binstr
              | atom
              | {enum, [atom()]}
              | optnl
              | nullable_integer
              | nullable_float
              | nullable_number
              | nullable_binstr
              .

-type reason() :: {custom, term()}
                | {unknown_rule, rule()}
                | {invalid, name(), input()}
                | {invalid_enum, [atom()], input()}
                .

-type result() :: valid
                | {normalized, output()}
                | {normalized, output(), reason()}
                | reject
                | {reject, reason()}
                .

-spec validate(rule(), input()) -> output().
validate({custom, Name, Custom, Acc}, Input) ->
    case Custom(Input, Acc) of
        valid ->
            ok;
        {normalized, _} ->
            error({?MODULE, {invalid, Name, Input}});
        {normalized, _, Reason} ->
            error({?MODULE, Reason});
        reject ->
            error({?MODULE, {invalid, Name, Input}});
        {reject, Reason} ->
            error({?MODULE, Reason})
    end;
validate(Rule, Input) ->
    case lookup_rule(Rule) of
        {value, {Custom, Acc}} ->
            validate({custom, Rule, Custom, Acc}, Input);
        none ->
            error({?MODULE, {unknown_rule, Rule}})
    end.

-spec normalize(rule(), input()) -> ok.
normalize({custom, Name, Custom, Acc}, Input) ->
    case Custom(Input, Acc) of
        valid ->
            Input;
        {normalized, Output} ->
            Output;
        {normalized, Output, _Reason} ->
            Output;
        reject ->
            error({?MODULE, {invalid, Name, Input}});
        {reject, Reason} ->
            error({?MODULE, Reason})
    end;
normalize(Rule, Input) ->
    case lookup_rule(Rule) of
        {value, {Custom, Acc}} ->
            normalize({custom, Rule, Custom, Acc}, Input);
        none ->
            error({?MODULE, {unknown_rule, Rule}})
    end.

-spec any_rule(input(), acc()) -> result().
any_rule(_Input, _Acc) ->
    valid.

-spec boolean_rule(input(), acc()) -> result().
boolean_rule(Input, _Acc) ->
    do(
        fun is_boolean/1
      , [fun
            (I) when -1 < I, I < 1 ->
                false;
            (I) ->
                case klsn_binstr:from_any(I) of
                    <<>> -> false;
                    <<"false">> -> false;
                    <<"False">> -> false;
                    <<"FALSE">> -> false;
                    <<"０"/utf8>> -> false;
                    <<"null">> -> false;
                    <<"Null">> -> false;
                    <<"NULL">> -> false;
                    <<"undefined">> -> false;
                    _ -> true
                end
        end]
      , Input
    ) .

-spec integer_rule(input(), acc()) -> result().
integer_rule(Input, _Acc) ->
    do(
        fun is_integer/1
      , [fun binary_to_integer/1, fun list_to_integer/1]
      , Input
    ) .

-spec float_rule(input(), acc()) -> result().
float_rule(Input, _Acc) ->
    do(
        fun is_float/1
      , [fun binary_to_float/1, fun list_to_float/1]
      , Input
    ) .

-spec number_rule(input(), acc()) -> result().
number_rule(Input, _Acc) ->
    do(
        fun is_number/1
      , [
            fun binary_to_integer/1
          , fun binary_to_float/1
          , fun list_to_integer/1
          , fun list_to_float/1
        ]
      , Input
    ) .

-spec timeout_rule(input(), acc()) -> result().
timeout_rule(Input, _Acc) ->
    do(
        fun
            (infinity) ->
                true;
            (I) when is_integer(I), I >= 0 ->
                true;
            (_) ->
                false
        end
      , [
            fun binary_to_integer/1
          , fun binary_to_float/1
          , fun list_to_integer/1
          , fun list_to_float/1
          , fun
                (<<"infinity">>) ->
                    infinity;
                ("infinity") ->
                    infinity
            end
        ]
      , Input
    ) .

-spec binstr_rule(input(), acc()) -> result().
binstr_rule(Input, _Acc) ->
    do(
        [fun klsn_binstr:from_any/1]
      , Input
    ) .
    
atom_rule(Input, none) ->
    do(
        fun is_atom/1
      , [fun(I) ->
            binary_to_existing_atom(klsn_binstr:from_any(I))
        end]
      , Input
    ) .

-spec enum_rule(input(), acc()) -> result().
enum_rule(Input, AllowedEnums) when is_list(AllowedEnums) ->
    InputBinary = try klsn_binstr:from_any(Input) catch
        _:_ ->
            error
    end,
    MaybeEnum = lists:search(fun(E) ->
        try
            InputBinary =:= klsn_binstr:from_any(E)
        catch _:_ ->
            false
        end
    end, AllowedEnums),
    case MaybeEnum of
        {value, Enum} when Input =:= Enum ->
            valid;
        {value, Enum} ->
            {normalized, Enum};
        _ ->
            {reject, {invalid_enum, AllowedEnums, Input}}
    end;
enum_rule(_, _) ->
    reject.

-spec optnl_rule(input(), acc()) -> result().
optnl_rule(none, _Acc) ->
    valid;
optnl_rule({value, _}, _Acc) ->
    valid;
optnl_rule(null, _Acc) ->
    {normalized, none};
optnl_rule(nil, _Acc) ->
    {normalized, none};
optnl_rule(undefined, _Acc) ->
    {normalized, none};
optnl_rule(false, _Acc) ->
    {normalized, none};
optnl_rule([], _Acc) ->
    {normalized, none};
optnl_rule(error, _Acc) ->
    {normalized, none};
optnl_rule({ok, Value}, _Acc) ->
    {normalized, {value, Value}};
optnl_rule({true, Value}, _Acc) ->
    {normalized, {value, Value}};
optnl_rule([Value], _Acc) ->
    {normalized, {value, Value}};
optnl_rule(Value, _Acc) when is_binary(Value) ->
    {normalized, {value, Value}};
optnl_rule(Value, _Acc) when is_number(Value) ->
    {normalized, {value, Value}};
optnl_rule(_, _Acc) ->
    reject.

-spec nullable_integer_rule(input(), acc()) -> result().
nullable_integer_rule(null, _Acc) ->
    valid;
nullable_integer_rule(none, _Acc) ->
    {normalized, null};
nullable_integer_rule({value, Value}, Acc) ->
    case integer_rule(Value, Acc) of
        valid ->
            {normalized, Value};
        Result ->
            Result
    end;
nullable_integer_rule(Value, Acc) ->
    integer_rule(Value, Acc).

-spec nullable_float_rule(input(), acc()) -> result().
nullable_float_rule(null, _Acc) ->
    valid;
nullable_float_rule(none, _Acc) ->
    {normalized, null};
nullable_float_rule({value, Value}, Acc) ->
    case float_rule(Value, Acc) of
        valid ->
            {normalized, Value};
        Result ->
            Result
    end;
nullable_float_rule(Value, Acc) ->
    float_rule(Value, Acc).

-spec nullable_number_rule(input(), acc()) -> result().
nullable_number_rule(null, _Acc) ->
    valid;
nullable_number_rule(none, _Acc) ->
    {normalized, null};
nullable_number_rule({value, Value}, Acc) ->
    case number_rule(Value, Acc) of
        valid ->
            {normalized, Value};
        Result ->
            Result
    end;
nullable_number_rule(Value, Acc) ->
    number_rule(Value, Acc).

-spec nullable_binstr_rule(input(), acc()) -> result().
nullable_binstr_rule(null, _Acc) ->
    valid;
nullable_binstr_rule(none, _Acc) ->
    {normalized, null};
nullable_binstr_rule({value, Value}, Acc) ->
    case binstr_rule(Value, Acc) of
        valid ->
            {normalized, Value};
        Result ->
            Result
    end;
nullable_binstr_rule(Value, Acc) ->
    binstr_rule(Value, Acc).

-spec do(fun((input()) -> boolean()), [fun((input()) -> output())], input()) -> result().
do(Guard, Converts, Input) ->
    try Guard(Input) of
        true ->
            valid;
        false ->
            do(Converts, Input)
    catch _:_ ->
        do(Converts, Input)
    end.

-spec do([fun((input()) -> output())], input()) -> result().
do([], _Input) ->
    reject;
do([H|T], Input) ->
    try H(Input) of
        Output when Output =:= Input ->
            valid;
        Output ->
            {normalized, Output}
    catch _:_ ->
        do(T, Input)
    end.

-spec lookup_rule(rule()) -> klsn:optnl({custom(), acc()}).
lookup_rule(Rule) ->
    {Name, Acc} = case Rule of
        {_, _} ->
            Rule;
        Name0 ->
            {Name0, []}
    end,
    MaybeRule = lists:search(fun
        ({FunName, 2})->
            try
                <<(klsn_binstr:from_any(Name))/binary, "_rule">> =:= klsn_binstr:from_any(FunName)
            catch _:_ ->
                false
            end;
        (_) ->
            false
    end, ?MODULE:module_info(exports)),
    case MaybeRule of
        {value, {Function, Arity}} ->
            {value, {fun ?MODULE:Function/Arity, Acc}};
        _ ->
            none
    end.
