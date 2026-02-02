-module(klsn_rule).

%% public functions
-export([
        validate/2
      , normalize/2
      , eval/2
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
      , list_rule/2
      , tuple_rule/2
      , map_rule/2
      , struct_rule/2
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
      , strict_result/0
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
              | {list, rule()}
              | {tuple, [rule()] | tuple()} % {rule(), rule(), ...}
              | {map, {KeyRule::rule(), ValueRule::rule()}}
              | {struct, #{atom() => {required | optional, rule()}}}
              .

-type reason() :: {custom, term()}
                | {unknown_rule, rule()}
                | {invalid, name(), input()}
                | {invalid_enum, [atom()], input()}
                | {invalid_list_element, pos_integer(), reason()}
                | {invalid_tuple_size, non_neg_integer(), input()}
                | {invalid_tuple_element, pos_integer(), reason()}
                | {invalid_map_key, reason()}
                | {invalid_map_value, Key::term(), reason()}
                | {map_key_conflict, Key::term()}
                | {invalid_struct_field, term()}
                | {invalid_struct_value, atom(), reason()}
                | {missing_required_field, atom()}
                | {struct_field_conflict, atom()}
                .

-type result() :: valid
                | {valid, output()}
                | {normalized, output()}
                | {normalized, output(), reason()}
                | reject
                | {reject, reason()}
                .

-type strict_result() :: {valid, output()}
                       | {normalized, output(), reason()}
                       | {reject, reason()}
                       .

-spec validate(rule(), input()) -> output().
validate(Rule, Input) ->
    case eval(Rule, Input) of
        {valid, _} ->
            ok;
        {normalized, _, Reason} ->
            error({?MODULE, Reason});
        {reject, Reason} ->
            error({?MODULE, Reason})
    end.

-spec normalize(rule(), input()) -> output().
normalize(Rule, Input) ->
    case eval(Rule, Input) of
        {valid, Output} ->
            Output;
        {normalized, Output, _Reason} ->
            Output;
        {reject, Reason} ->
            error({?MODULE, Reason})
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
    
atom_rule(Input, _Acc) ->
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

-spec list_rule(input(), acc()) -> result().
list_rule(Input, ElementRule) when is_list(Input) ->
    List0 = lists:map(fun(Elem) ->
        eval(ElementRule, Elem)
    end, Input),
    List = lists:zip(lists:seq(1, length(List0)), List0),
    MaybeReject = lists:search(fun
        ({_I, {reject, _}})->
            true;
        (_) ->
            false
    end, List),
    case MaybeReject of
        {value, {I, {reject, Reason}}} ->
            {reject, {invalid_list_element, I, Reason}};
        _ ->
            MaybeNormalized = lists:search(fun
                ({_I, {normalized, _, _}})->
                    true;
                (_) ->
                    false
            end, List),
            case MaybeNormalized of
                {value, {I, {normalized, _, Reason}}} ->
                    Output = lists:map(fun
                        ({_I, {valid, ElemOutput}}) ->
                            ElemOutput;
                        ({_I, {normalized, ElemOutput, _}}) ->
                            ElemOutput
                    end, List),
                    {normalized, Output, {invalid_list_element, I, Reason}};
                _ ->
                    valid
            end
    end;
list_rule(_, _) ->
    reject.

-spec tuple_rule(input(), acc()) -> result().
tuple_rule(Input, Rules) when is_tuple(Input), is_tuple(Rules) ->
    tuple_rule(Input, tuple_to_list(Rules));
tuple_rule(Input, Rules) when is_tuple(Input), is_list(Rules), length(Rules) =/= tuple_size(Input) ->
    {reject, {invalid_tuple_size, length(Rules), Input}};
tuple_rule(Input, Rules) when is_tuple(Input), is_list(Rules) ->
    InputList = tuple_to_list(Input),
    List0 = lists:map(fun({Rule, Elem}) ->
        eval(Rule, Elem)
    end, lists:zip(Rules, InputList)),
    List = lists:zip(lists:seq(1, length(List0)), List0),
    MaybeReject = lists:search(fun
        ({_I, {reject, _}})->
            true;
        (_) ->
            false
    end, List),
    case MaybeReject of
        {value, {I, {reject, Reason}}} ->
            {reject, {invalid_tuple_element, I, Reason}};
        _ ->
            MaybeNormalized = lists:search(fun
                ({_I, {normalized, _, _}})->
                    true;
                (_) ->
                    false
            end, List),
            case MaybeNormalized of
                {value, {I, {normalized, _, Reason}}} ->
                    Output = lists:map(fun
                        ({_I, {valid, ElemOutput}}) ->
                            ElemOutput;
                        ({_I, {normalized, ElemOutput, _}}) ->
                            ElemOutput
                    end, List),
                    {normalized, list_to_tuple(Output), {invalid_tuple_element, I, Reason}};
                _ ->
                    valid
            end
    end;
tuple_rule(_, _) ->
    reject.

-spec map_rule(input(), acc()) -> result().
map_rule(Input, {KeyRule, ValueRule}) when is_map(Input) ->
    List0 = lists:map(fun({Key, Value}) ->
        {Key, eval(KeyRule, Key), eval(ValueRule, Value)}
    end, maps:to_list(Input)),
    MaybeKeyReject = lists:search(fun
        ({_Key, {reject, _}, _}) ->
            true;
        (_) ->
            false
    end, List0),
    case MaybeKeyReject of
        {value, {_Key, {reject, Reason}, _}} ->
            {reject, {invalid_map_key, Reason}};
        _ ->
            MaybeValueReject = lists:search(fun
                ({_Key, _KeyRes, {reject, _}}) ->
                    true;
                (_) ->
                    false
            end, List0),
            case MaybeValueReject of
                {value, {Key, _KeyRes, {reject, Reason}}} ->
                    {reject, {invalid_map_value, Key, Reason}};
                _ ->
                    MaybeNormalized = lists:search(fun
                        ({_Key, {normalized, _, _}, _}) ->
                            true;
                        ({_Key, _KeyRes, {normalized, _, _}}) ->
                            true;
                        (_) ->
                            false
                    end, List0),
                    MaybeNormReason = case MaybeNormalized of
                        {value, {_Key, {normalized, _, Reason}, _}} ->
                            {value, {invalid_map_key, Reason}};
                        {value, {Key, _KeyRes, {normalized, _, Reason}}} ->
                            {value, {invalid_map_value, Key, Reason}};
                        _ ->
                            none
                    end,
                    case MaybeNormReason of
                        {value, NormReason} ->
                            OutputList = lists:map(fun({_Key0, KeyRes0, ValueRes0}) ->
                                Key1 = case KeyRes0 of
                                    {valid, KeyOut} ->
                                        KeyOut;
                                    {normalized, KeyOut, _} ->
                                        KeyOut
                                end,
                                Value1 = case ValueRes0 of
                                    {valid, ValueOut} ->
                                        ValueOut;
                                    {normalized, ValueOut, _} ->
                                        ValueOut
                                end,
                                {Key1, Value1}
                            end, List0),
                            MaybeKeyConflict = lists:foldl(fun({Key1, _Value1}, Acc) ->
                                case Acc of
                                    {value, _} ->
                                        Acc;
                                    Seen ->
                                        case maps:is_key(Key1, Seen) of
                                            true ->
                                                {value, Key1};
                                            false ->
                                                maps:put(Key1, true, Seen)
                                        end
                                end
                            end, #{}, OutputList),
                            case MaybeKeyConflict of
                                {value, Key1} ->
                                    {reject, {map_key_conflict, Key1}};
                                _ ->
                                    {normalized, maps:from_list(OutputList), NormReason}
                            end;
                        _ ->
                            valid
                    end
            end
    end;
map_rule(_, _) ->
    reject.

-spec struct_rule(input(), acc()) -> result().
struct_rule(Input, StructSpec) when is_map(Input), is_map(StructSpec) ->
    SpecList0 = maps:to_list(StructSpec),
    SpecList = lists:map(fun
        ({Field, {ReqOpt, Rule}}) when is_atom(Field), (ReqOpt =:= required orelse ReqOpt =:= optional) ->
            {Field, {ReqOpt, Rule}};
        (_) ->
            error
    end, SpecList0),
    case lists:member(error, SpecList) of
        true ->
            reject;
        false ->
            BinToField = maps:from_list(lists:map(fun({Field, _}) ->
                {klsn_binstr:from_any(Field), Field}
            end, SpecList)),
            InputList = maps:to_list(Input),
            {Matches0, ExtraKeysRev, KeyNormKeysRev} = lists:foldl(fun({Key, Value}, {MatchesAcc, ExtraAcc, NormAcc}) ->
                case maybe_binstr_from_any_(Key) of
                    {value, KeyBin} ->
                        case maps:find(KeyBin, BinToField) of
                            {ok, Field} ->
                                Existing = maps:get(Field, MatchesAcc, []),
                                MatchesAcc1 = maps:put(Field, [{Key, Value}|Existing], MatchesAcc),
                                NormAcc1 = case Key =:= Field of
                                    true ->
                                        NormAcc;
                                    false ->
                                        [Key|NormAcc]
                                end,
                                {MatchesAcc1, ExtraAcc, NormAcc1};
                            error ->
                                {MatchesAcc, [Key|ExtraAcc], NormAcc}
                        end;
                    none ->
                        {MatchesAcc, [Key|ExtraAcc], NormAcc}
                end
            end, {#{}, [], []}, InputList),
            ExtraKeys = lists:reverse(ExtraKeysRev),
            KeyNormKeys = lists:reverse(KeyNormKeysRev),
            MaybeConflict = lists:search(fun({Field, _}) ->
                case maps:get(Field, Matches0, []) of
                    [] -> false;
                    [_] -> false;
                    [_|_] -> true
                end
            end, SpecList),
            case MaybeConflict of
                {value, {Field, _}} ->
                    {reject, {struct_field_conflict, Field}};
                _ ->
                    MaybeMissingRequired = lists:search(fun
                        ({Field, {required, _}}) ->
                            maps:get(Field, Matches0, []) =:= [];
                        (_) ->
                            false
                    end, SpecList),
                    case MaybeMissingRequired of
                        {value, {Field, _}} ->
                            {reject, {missing_required_field, Field}};
                        _ ->
                            case struct_eval_fields_(SpecList, Matches0, #{}, none) of
                                {reject, Reason} ->
                                    {reject, Reason};
                                {ok, Output, MaybeValueNormReason} ->
                                    IsNormalized = (ExtraKeys =/= []) orelse (KeyNormKeys =/= []) orelse (MaybeValueNormReason =/= none),
                                    case IsNormalized of
                                        false ->
                                            valid;
                                        true ->
                                            NormReason = case ExtraKeys of
                                                [ExtraKey|_] ->
                                                    {invalid_struct_field, ExtraKey};
                                                [] ->
                                                    case KeyNormKeys of
                                                        [KeyNormKey|_] ->
                                                            {invalid_struct_field, KeyNormKey};
                                                        [] ->
                                                            klsn_maybe:get_value(MaybeValueNormReason)
                                                    end
                                            end,
                                            {normalized, Output, NormReason}
                                    end
                            end
                    end
            end
    end;
struct_rule(_, _) ->
    reject.

-spec maybe_binstr_from_any_(term()) -> klsn:optnl(klsn:binstr()).
maybe_binstr_from_any_(Value) ->
    try
        {value, klsn_binstr:from_any(Value)}
    catch
        _:_ ->
            none
    end.

-spec struct_eval_fields_(
        [{atom(), {required | optional, rule()}}]
      , #{atom() => [{term(), term()}]}
      , maps:map(atom(), term())
      , klsn:optnl(reason())
    ) -> {ok, maps:map(atom(), term()), klsn:optnl(reason())} | {reject, reason()}.
struct_eval_fields_([], _Matches, Output, MaybeNormReason) ->
    {ok, Output, MaybeNormReason};
struct_eval_fields_([{Field, {_ReqOpt, Rule}}|T], Matches, Output0, MaybeNormReason0) ->
    case maps:get(Field, Matches, []) of
        [] ->
            struct_eval_fields_(T, Matches, Output0, MaybeNormReason0);
        [{_Key, Value}] ->
            case eval(Rule, Value) of
                {valid, ValueOut} ->
                    struct_eval_fields_(T, Matches, maps:put(Field, ValueOut, Output0), MaybeNormReason0);
                {normalized, ValueOut, Reason} ->
                    MaybeNormReason1 = case MaybeNormReason0 of
                        none ->
                            {value, {invalid_struct_value, Field, Reason}};
                        _ ->
                            MaybeNormReason0
                    end,
                    struct_eval_fields_(T, Matches, maps:put(Field, ValueOut, Output0), MaybeNormReason1);
                {reject, Reason} ->
                    {reject, {invalid_struct_value, Field, Reason}}
            end;
        _ ->
            {reject, {struct_field_conflict, Field}}
    end.

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


-spec eval(rule(), input()) -> strict_result().
eval({custom, Name, Custom, Acc}=Arg1, Input) ->
    case Custom(Input, Acc) of
        valid ->
            {valid, Input};
        {valid, Input} ->
            {valid, Input};
        {valid, Modified} ->
            error({invalid_custom_rule, klsn_binstr:from_any(io_lib:format(
                "custom rule ~p returned {valid, Output}=~p for input ~p, but Output /= Input. "
                "{valid, output()} must return the unmodified input (or return valid)."
              , [Name, Modified, Input]
            ))}, [Arg1, Input]);
        {normalized, Output} ->
            {normalized, Output, {invalid, Name, Input}};
        {normalized, Output, Reason} ->
            {normalized, Output, Reason};
        reject ->
            {reject, {invalid, Name, Input}};
        {reject, Reason} ->
            {reject, Reason};
        Unexpected ->
            error({invalid_custom_rule, klsn_binstr:from_any(io_lib:format(
                "custom rule ~p returned ~p when input is ~p. klsn_rule:result() expected."
              , [Name, Unexpected, Input]
            ))}, [Arg1, Input])
    end;
eval(Rule, Input) ->
    case rule_to_custom_(Rule) of
        {value, CustomRule} ->
            eval(CustomRule, Input);
        none ->
            {reject, {unknown_rule, Rule}}
    end.


-spec rule_to_custom_(rule()) -> klsn:optnl({custom, name(), custom(), acc()}).
rule_to_custom_({custom, Name, Custom, Acc}) ->
    {value, {custom, Name, Custom, Acc}};
rule_to_custom_(Rule) ->
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
            {value, {custom, Name, fun ?MODULE:Function/Arity, Acc}};
        _ ->
            none
    end.
