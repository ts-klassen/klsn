-module(klsn_rule).

%% public functions
-export([
        validate/2
      , normalize/2
      , eval/2
      , lookup_alias/1
    ]).

%% builtin rules
-export([
        term_rule/2
      , exact_rule/2
      , default_rule/2
      , boolean_rule/2
      , integer_rule/2
      , float_rule/2
      , number_rule/2
      , range_rule/2
      , alias_rule/2
      , timeout_rule/2
      , binstr_rule/2
      , atom_rule/2
      , enum_rule/2
      , any_of_rule/2
      , all_of_rule/2
      , foldl_rule/2
      , optnl_rule/2
      , nullable_rule/2
      , strict_rule/2
      , list_rule/2
      , tuple_rule/2
      , map_rule/2
      , struct_rule/2
    ]).

-export_type([
        name/0
      , input/0
      , output/0
      , alias/0
      , alias/1
      , alias/2
      , alias_ref/0
      , custom/0
      , rule_param/0
      , rule/0
      , reason/0
      , result/0
      , strict_result/0
    ]).

-type name() :: atom().

-type input() :: term().

-type output() :: term().

-type alias() :: atom().

%% Just to define a type by using an rule alias.
-type alias(_AliasName) :: term().
-type alias(_Module, _AliasName) :: term().

-type alias_ref() :: {module(), alias()}.

-type custom() :: fun( (input(), rule_param()) -> result() ).

-type rule_param() :: term().

-type rule() :: {custom, name(), custom(), rule_param()}
              | term
              | {exact, term()}
              | {default, {output(), rule()}}
              | boolean
              | integer
              | float
              | number
              | {range, range_(rule())}
              | {alias, alias_ref()}
              | timeout
              | binstr
              | atom
              | {enum, [atom()]}
              | {any_of, [rule()]}
              | {all_of, [rule()]}
              | {foldl, [rule()]}
              | {optnl, rule()}
              | {nullable, rule()}
              | {strict, rule()}
              | {list, rule()}
              | {tuple, [rule()] | tuple()} % {rule(), rule(), ...}
              | {map, {KeyRule::rule(), ValueRule::rule()}}
              | {struct, #{atom() => {required | optional, rule()}}}
              .

-type reason() :: {custom, term()}
                | {unknown_rule, rule()}
                | {invalid, name(), input()}
                | {invalid_exact, term(), input()}
                | {invalid_enum, [atom()], input()}
                | {invalid_list_element, pos_integer(), reason()}
                | {invalid_tuple_size, non_neg_integer(), input()}
                | {invalid_tuple_element, pos_integer(), reason()}
                | {invalid_optnl_value, reason()}
                | {invalid_nullable_value, reason()}
                | {invalid_map_key, reason()}
                | {invalid_map_value, Key::term(), reason()}
                | {map_key_conflict, Key::term()}
                | {invalid_alias, alias_ref(), reason()}
                | {undefined_alias, alias_ref(), input()}
                | {invalid_struct_field, term()}
                | {invalid_struct_value, atom(), reason()}
                | {missing_required_field, atom()}
                | {struct_field_conflict, atom()}
                | {any_of, [reason()]}
                | {all_of, [reason()]}
                | {strict, reason()}
                | {invalid_range, range_(input())}
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

-type range_(Subject) :: {number(), '<' | '=<', Subject}
                       | {Subject, '<' | '=<', number()}
                       | {number(), '<' | '=<', Subject, '<' | '=<', number()}
                       .

%% @doc
%% Lookup a named rule alias declared via -klsn_rule_alias in a module.
%%
%% Returns {value, Rule} when found, otherwise none.
%%
%% Examples:
%% ```
%% 1> klsn_rule:lookup_alias({my_mod, my_alias}).
%% {value, integer}
%% 2> klsn_rule:lookup_alias({my_mod, missing}).
%% none
%% '''
-spec lookup_alias(alias_ref()) -> klsn:optnl(rule()).
lookup_alias({Module, Alias}) when is_atom(Module), is_atom(Alias) ->
    try Module:module_info(attributes) of
        Attrs ->
            AliasRules0 = proplists:lookup_all(klsn_rule_alias, Attrs),
            AliasRules1 = lists:map(fun
                ({_, RuleEntries}) when is_list(RuleEntries) ->
                    RuleEntries;
                ({_, RuleEntry}) ->
                    [RuleEntry]
            end, AliasRules0),
            AliasRules = lists:concat(AliasRules1),
            case lists:search(fun
                ({RuleName0, _Rule0}) when RuleName0 =:= Alias ->
                    true;
                (_) ->
                    false
            end, AliasRules) of
                {value, {_, RuleValue}} ->
                    {value, RuleValue};
                false ->
                    none
            end
    catch _:_ ->
        none
    end;
lookup_alias(_Arg1) ->
    none.

%% @doc
%% Validate an input against a rule.
%%
%% This calls {@link eval/2} and only accepts a `{valid, Output}'
%% result. Any normalized or reject result raises
%% `error({klsn_rule, Reason})'.
%%
%% Examples:
%% ```
%% 1> klsn_rule:validate(integer, 10).
%% ok
%% 2> klsn_rule:validate(integer, <<"10">>).
%% ** exception error: {klsn_rule,{invalid,integer,<<"10">>}}
%% '''
%% @see normalize/2
%% @see eval/2
-spec validate(rule(), input()) -> ok.
validate(Rule, Input) ->
    case eval(Rule, Input) of
        {valid, _} ->
            ok;
        {normalized, _, Reason} ->
            error({?MODULE, Reason});
        {reject, Reason} ->
            error({?MODULE, Reason})
    end.

%% @doc
%% Normalize an input according to a rule.
%%
%% This calls {@link eval/2}. Valid and normalized results return the output,
%% while reject results raise `error({klsn_rule, Reason})'.
%% Normalization reasons are dropped; use {@link eval/2} if you need them.
%%
%% Examples:
%% ```
%% 1> klsn_rule:normalize(integer, <<"10">>).
%% 10
%% 2> klsn_rule:normalize({range, {0, '=<', integer, '<', 10}}, 5).
%% 5
%% 3> klsn_rule:normalize({range, {0, '=<', integer, '<', 10}}, 99).
%% ** exception error: {klsn_rule,{invalid_range,{0,'=<',99,'<',10}}}
%% '''
%% @see validate/2
%% @see eval/2
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

%% @doc
%% Accept any input and always return valid for validate/2, normalize/2, and eval/2.
%% Rule form: term (rule()).
%%
%% Result (eval/2):
%% - valid with the input unchanged.
%% - never normalized (no reason()).
%% - never rejected (no reason()).
%%
%% Examples:
%% ```
%% 1> klsn_rule:validate(term, {any, value}).
%% ok
%% 2> klsn_rule:normalize(term, 123).
%% 123
%% 3> klsn_rule:eval(term, [a, b]).
%% {valid, [a, b]}
%% '''
%% @see validate/2
%% @see normalize/2
%% @see eval/2
-spec term_rule(input(), rule_param()) -> result().
term_rule(_Input, _Param) ->
    valid.

%% @doc
%% Match an input against the exact rule parameter value.
%%
%% Rule form: {exact, Exact}.
%%
%% When evaluated via {@link eval/2}:
%% - valid: {valid, Input} when Input =:= Exact
%% - reject: {reject, {invalid_exact, Exact, Input}}
%%
%% This rule never produces a normalized result. {@link normalize/2} either
%% returns the original input or raises with {klsn_rule, {invalid_exact, Exact, Input}}.
%%
%% Examples:
%% ```
%% 1> klsn_rule:eval({exact, 42}, 42).
%% {valid, 42}
%% 2> klsn_rule:eval({exact, 42}, 7).
%% {reject, {invalid_exact, 42, 7}}
%% 3> klsn_rule:normalize({exact, ok}, ok).
%% ok
%% 4> klsn_rule:normalize({exact, ok}, error).
%% ** exception error: {klsn_rule,{invalid_exact,ok,error}}
%% 5> klsn_rule:validate({exact, ok}, ok).
%% ok
%% '''
%% @see validate/2
%% @see normalize/2
%% @see eval/2
-spec exact_rule(input(), rule_param()) -> result().
exact_rule(Input, Exact) ->
    case Input =:= Exact of
        true ->
            valid;
        false ->
            {reject, {invalid_exact, Exact, Input}}
    end.

%% @doc
%% Apply Rule and fall back to Default when it rejects.
%%
%% Rule form: {default, {Default, Rule}} where Default is output() and
%% Rule is rule().
%%
%% When evaluated via {@link eval/2}:
%% - valid: {valid, Output} when Rule returns {valid, Output}.
%% - normalized: {normalized, Output, Reason} when Rule returns
%%   {normalized, Output, Reason}.
%% - normalized: {normalized, Default, Reason} when Rule returns
%%   {reject, Reason}.
%% - reject: {reject, {invalid, default, Input}} when the rule parameter is not
%%   {Default, Rule}.
%%
%% Reason handling:
%% - Reasons are passed through from Rule (including {unknown_rule, Rule}).
%% - If Rule normalizes without a reason, {@link eval/2} uses
%%   {invalid, RuleName, Input} where RuleName is the inner rule name.
%% - {@link normalize/2} returns Output or Default and drops the reason.
%% - {@link validate/2} raises error({klsn_rule, Reason}) on normalized or
%%   reject results.
%%
%% Examples:
%% ```
%% 1> klsn_rule:eval({default, {ok, {exact, ok}}}, ok).
%% {valid, ok}
%% 2> klsn_rule:eval({default, {ok, {exact, ok}}}, error).
%% {normalized, ok, {invalid_exact, ok, error}}
%% 3> klsn_rule:normalize({default, {0, {exact, 0}}}, 1).
%% 0
%% 4> klsn_rule:validate({default, {0, {exact, 0}}}, 1).
%% ** exception error: {klsn_rule,{invalid_exact,0,1}}
%% 5> klsn_rule:eval({default, []}, 1).
%% {reject, {invalid, default, 1}}
%% '''
%% @see validate/2
%% @see normalize/2
%% @see eval/2
-spec default_rule(input(), rule_param()) -> result().
default_rule(Input, {Default, Rule}) ->
    case eval(Rule, Input) of
        {valid, Output} ->
            {valid, Output};
        {normalized, Output, Reason} ->
            {normalized, Output, Reason};
        {reject, Reason} ->
            {normalized, Default, Reason}
    end;
default_rule(_, _) ->
    reject.

%% @doc
%% Normalize booleans from common boolean-like inputs.
%%
%% Rule form: boolean.
%%
%% Accepted as-is: true and false.
%% Otherwise, values are coerced by:
%% - returning false for numbers with `-1 < N < 1';
%% - converting via klsn_binstr:from_any/1 and treating
%%   `<<>>', "false"/"False"/"FALSE", "null"/"Null"/"NULL", the
%%   lowercase "undefined", and the Unicode fullwidth zero (U+FF10)
%%   as false; everything else is true.
%%
%% When evaluated via {@link eval/2}:
%% - valid: {valid, Input} when Input is boolean
%% - normalized: {normalized, Bool, {invalid, boolean, Input}} when coerced
%% - reject: {reject, {invalid, boolean, Input}} when not coercible
%%
%% validate/2 only accepts literal booleans; any coercion or reject
%% yields error({klsn_rule, {invalid, boolean, Input}}). Use
%% normalize/2 to allow coercion.
%%
%% Examples:
%% ```
%% 1> klsn_rule:eval(boolean, true).
%% {valid, true}
%% 2> klsn_rule:eval(boolean, <<"false">>).
%% {normalized, false, {invalid, boolean, <<"false">>}}
%% 3> klsn_rule:normalize(boolean, 0).
%% false
%% 4> klsn_rule:eval(boolean, <<"0">>).
%% {normalized, true, {invalid, boolean, <<"0">>}}
%% 5> klsn_rule:eval(boolean, #{}).
%% {reject, {invalid, boolean, #{}}}
%% 6> klsn_rule:validate(boolean, <<"false">>).
%% ** exception error: {klsn_rule,{invalid,boolean,<<"false">>}}
%% '''
%% @see validate/2
%% @see normalize/2
%% @see eval/2
-spec boolean_rule(input(), rule_param()) -> result().
boolean_rule(Input, _Param) ->
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

%% @doc
%% Validate integer input for use with validate/2, normalize/2, and eval/2.
%% Rule form: integer.
%%
%% Result (eval/2):
%% - valid when Input is an integer.
%% - normalized when Input is a list or binary that parses via
%%   binary_to_integer/1 or list_to_integer/1.
%% - reject when Input is neither an integer nor a parseable list/binary.
%%
%% Reason (eval/2):
%% - {invalid, integer, Input} on normalize and reject.
%%   validate/2 raises error({klsn_rule, {invalid, integer, Input}}) when
%%   normalization or rejection occurs.
%%
%% Examples:
%% ```
%% 1> klsn_rule:eval(integer, 10).
%% 2> klsn_rule:eval(integer, "10").
%% 3> klsn_rule:normalize(integer, "10").
%% 4> klsn_rule:eval(integer, "nope").
%% '''
%% @see validate/2
%% @see normalize/2
%% @see eval/2
-spec integer_rule(input(), rule_param()) -> result().
integer_rule(Input, _Param) ->
    do(
        fun is_integer/1
      , [fun binary_to_integer/1, fun list_to_integer/1]
      , Input
    ) .

%% @doc
%% Validate float input for validate/2, normalize/2, and eval/2.
%% Rule form: float.
%%
%% Result (eval/2):
%% - valid when Input is a float.
%% - normalized when Input is a binary or list that parses via binary_to_float/1
%%   or list_to_float/1; reason is {invalid, float, Input}.
%% - reject when Input cannot be converted; reason is {invalid, float, Input}.
%%
%% normalize/2 returns the float output (original or parsed) and drops the reason.
%% validate/2 accepts only valid results; normalized or reject raise
%% error({klsn_rule, {invalid, float, Input}}).
%%
%% Examples:
%% ```
%% 1> klsn_rule:eval(float, 1.25).
%% {valid, 1.25}
%% 2> klsn_rule:eval(float, <<"1.25">>).
%% {normalized, 1.25, {invalid, float, <<"1.25">>}}
%% 3> klsn_rule:normalize(float, "1.25").
%% 1.25
%% 4> klsn_rule:eval(float, <<"nope">>).
%% {reject, {invalid, float, <<"nope">>}}
%% '''
%% @see validate/2
%% @see normalize/2
%% @see eval/2
-spec float_rule(input(), rule_param()) -> result().
float_rule(Input, _Param) ->
    do(
        fun is_float/1
      , [fun binary_to_float/1, fun list_to_float/1]
      , Input
    ) .

%% @doc
%% Validate numeric input for validate/2, normalize/2, and eval/2.
%% Rule form: number (or {number, Param}; Param is ignored).
%%
%% Result (eval/2):
%% - valid when Input is a number (integer or float).
%% - normalized when Input is a binary or list that parses as an integer/float;
%%   parsing order is binary_to_integer/1, binary_to_float/1,
%%   list_to_integer/1, list_to_float/1.
%% - reject when Input is not numeric and cannot be parsed.
%%
%% Reason (eval/2):
%% - {invalid, number, Input} on normalize or reject.
%%
%% normalize/2 returns the parsed number (or original) or raises with
%% {klsn_rule, {invalid, number, Input}}. validate/2 returns ok only for
%% already-numeric input and raises for normalized/reject results.
%%
%% Examples:
%% ```
%% 1> klsn_rule:eval(number, 10).
%% {valid, 10}
%% 2> klsn_rule:eval(number, <<"10">>).
%% {normalized, 10, {invalid, number, <<"10">>}}
%% 3> klsn_rule:normalize(number, "1.5").
%% 1.5
%% 4> klsn_rule:eval(number, <<"nope">>).
%% {reject, {invalid, number, <<"nope">>}}
%% '''
%% @see validate/2
%% @see normalize/2
%% @see eval/2
-spec number_rule(input(), rule_param()) -> result().
number_rule(Input, _Param) ->
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

%% @doc
%% Validate a range rule used by validate/2, normalize/2, and eval/2.
%% Rule form: {range, range_(Rule)} where Rule is rule() and range_(Rule)
%% is one of:
%%   {Rule, Op, Upper} | {Lower, Op, Rule} | {Lower, Op1, Rule, Op2, Upper}
%% Op/Op1/Op2 are `` '<' '' or `` '=<' ''. Lower/Upper are numbers.
%%
%% Result (eval/2):
%% - valid when Rule validates and the output satisfies the bound(s).
%% - normalized with the same Reason when Rule normalizes and the output
%%   satisfies the bound(s).
%% - reject with {invalid_range, RangeOutput} when the bound check fails.
%% - reject with the underlying Reason when Rule rejects.
%%
%% normalize/2 returns Output for valid/normalized results and raises with
%% {klsn_rule, Reason} on reject. validate/2 returns ok only for valid results.
%%
%% Examples:
%% ```
%% 1> klsn_rule:eval({range, {integer, '=<', 10}}, <<"10">>).
%% 2> klsn_rule:eval({range, {0, '=<', integer, '<', 10}}, 5).
%% '''
%% @see validate/2
%% @see normalize/2
%% @see eval/2
-spec range_rule(input(), rule_param()) -> result().
range_rule(Input, {Subject, Op, Upper})
    when (Op =:= '<' orelse Op =:= '=<'),
         is_number(Upper),
         is_number(Subject) =:= false ->
    case eval(Subject, Input) of
        {valid, Output} ->
            case Op of
                '<' when Output < Upper ->
                    valid;
                '=<' when Output =< Upper ->
                    valid;
                _ ->
                    {reject, {invalid_range, {Output, Op, Upper}}}
            end;
        {normalized, Output, Reason} ->
            case Op of
                '<' when Output < Upper ->
                    {normalized, Output, Reason};
                '=<' when Output =< Upper ->
                    {normalized, Output, Reason};
                _ ->
                    {reject, {invalid_range, {Output, Op, Upper}}}
            end;
        {reject, Reason} ->
            {reject, Reason}
    end;
range_rule(Input, {Lower, Op, Subject})
    when (Op =:= '<' orelse Op =:= '=<'),
         is_number(Lower),
         is_number(Subject) =:= false ->
    case eval(Subject, Input) of
        {valid, Output} ->
            case Op of
                '<' when Lower < Output ->
                    valid;
                '=<' when Lower =< Output ->
                    valid;
                _ ->
                    {reject, {invalid_range, {Lower, Op, Output}}}
            end;
        {normalized, Output, Reason} ->
            case Op of
                '<' when Lower < Output ->
                    {normalized, Output, Reason};
                '=<' when Lower =< Output ->
                    {normalized, Output, Reason};
                _ ->
                    {reject, {invalid_range, {Lower, Op, Output}}}
            end;
        {reject, Reason} ->
            {reject, Reason}
    end;
range_rule(Input, {Lower, Op1, Subject, Op2, Upper})
    when (Op1 =:= '<' orelse Op1 =:= '=<'),
         (Op2 =:= '<' orelse Op2 =:= '=<'),
         is_number(Lower),
         is_number(Upper),
         is_number(Subject) =:= false ->
    case eval(Subject, Input) of
        {valid, Output} ->
            LowerOk = case Op1 of
                '<' when Lower < Output ->
                    true;
                '=<' when Lower =< Output ->
                    true;
                _ ->
                    false
            end,
            UpperOk = case Op2 of
                '<' when Output < Upper ->
                    true;
                '=<' when Output =< Upper ->
                    true;
                _ ->
                    false
            end,
            case LowerOk andalso UpperOk of
                true ->
                    valid;
                false ->
                    {reject, {invalid_range, {Lower, Op1, Output, Op2, Upper}}}
            end;
        {normalized, Output, Reason} ->
            LowerOk = case Op1 of
                '<' when Lower < Output ->
                    true;
                '=<' when Lower =< Output ->
                    true;
                _ ->
                    false
            end,
            UpperOk = case Op2 of
                '<' when Output < Upper ->
                    true;
                '=<' when Output =< Upper ->
                    true;
                _ ->
                    false
            end,
            case LowerOk andalso UpperOk of
                true ->
                    {normalized, Output, Reason};
                false ->
                    {reject, {invalid_range, {Lower, Op1, Output, Op2, Upper}}}
            end;
        {reject, Reason} ->
            {reject, Reason}
    end;
range_rule(_, _) ->
    reject.

%% @doc
%% Resolve and evaluate a named rule alias declared via -klsn_rule_alias.
%%
%% Rule form: {alias, AliasRef} where AliasRef is {Module, Alias}.
%%
%% Result (eval/2):
%% - valid when the named rule validates.
%% - normalized when the named rule normalizes; reason is
%%   {invalid_alias, AliasRef, Reason}.
%% - reject when the named rule rejects; reason is
%%   {invalid_alias, AliasRef, Reason}.
%% - reject with {undefined_alias, AliasRef, Input} when missing.
%% - reject with {invalid, alias, Input} when AliasRef is malformed.
%%
%% Examples:
%% ```
%% 1> MyAlias = {my_mod, my_alias}.
%% 2> klsn_rule:eval({alias, MyAlias}, 42).
%% {valid, 42}
%% 3> klsn_rule:eval({alias, MyAlias}, <<"42">>).
%% {normalized, 42, {invalid_alias, MyAlias, {invalid, integer, <<"42">>}}}
%% '''
%% @see lookup_alias/1
%% @see eval/2
-spec alias_rule(input(), rule_param()) -> result().
alias_rule(Input, {Module, Alias}=AliasRef)
    when is_atom(Module), is_atom(Alias) ->
    alias_rule_eval_(Input, AliasRef);
alias_rule(_, _) ->
    reject.

-spec alias_rule_eval_(input(), alias_ref()) -> result().
alias_rule_eval_(Input, AliasRef) ->
    case lookup_alias(AliasRef) of
        {value, Rule} ->
            case eval(Rule, Input) of
                {valid, Output} ->
                    {valid, Output};
                {normalized, Output, Reason} ->
                    {normalized, Output, {invalid_alias, AliasRef, Reason}};
                {reject, Reason} ->
                    {reject, {invalid_alias, AliasRef, Reason}}
            end;
        none ->
            {reject, {undefined_alias, AliasRef, Input}}
    end.

%% @doc
%% Validate timeout input for validate/2, normalize/2, and eval/2.
%% Rule form: timeout.
%%
%% Result (eval/2):
%% - valid when Input is infinity or a non-negative integer.
%% - normalized when Input is a list/binary that parses via
%%   binary_to_integer/1 or list_to_integer/1, or the string "infinity".
%% - reject when Input cannot be converted.
%%
%% Reason (eval/2):
%% - {invalid, timeout, Input} on normalize or reject.
%%
%% normalize/2 returns the converted timeout (or original) or raises with
%% {klsn_rule, {invalid, timeout, Input}}. validate/2 returns ok only when the
%% input is already valid and raises on normalization or reject.
%%
%% Examples:
%% ```
%% 1> klsn_rule:eval(timeout, infinity).
%% 2> klsn_rule:eval(timeout, 0).
%% 3> klsn_rule:eval(timeout, <<"15">>).
%% 4> klsn_rule:normalize(timeout, "15").
%% 5> klsn_rule:eval(timeout, foo).
%% '''
%% @see validate/2
%% @see normalize/2
%% @see eval/2
-spec timeout_rule(input(), rule_param()) -> result().
timeout_rule(Input, _Param) ->
    Rule = {any_of, [{enum, [infinity]}, {range, {0, '=<', integer}}]},
    case eval(Rule, Input) of
        {valid, _Output} ->
            valid;
        {normalized, Output, _Reason} ->
            {normalized, Output};
        {reject, _Reason} ->
            reject
    end.

%% @doc
%% Validate binary strings used by validate/2, normalize/2, and eval/2.
%% Rule form: binstr.
%%
%% Accepts binaries as-is (klsn:binstr()); otherwise converts integers,
%% floats, atoms, and iolists via klsn_binstr:from_any/1.
%%
%% Result:
%% - valid when Input is already a binary.
%% - normalized when Input converts to a binary; reason is {invalid, binstr, Input}.
%% - reject when conversion fails; reason is {invalid, binstr, Input}.
%%
%% {@link normalize/2} returns the binary (original or converted) or raises
%% error({klsn_rule, {invalid, binstr, Input}}). {@link validate/2} returns
%% ok only when the input is already a binary, otherwise it raises with
%% the same reason.
%%
%% Examples:
%% ```
%% 1> klsn_rule:eval(binstr, <<"ok">>).
%% 2> klsn_rule:eval(binstr, 42).
%% 3> klsn_rule:normalize(binstr, [<<"a">>, "b"]).
%% 4> klsn_rule:eval(binstr, #{}).
%% '''
%% @see validate/2
%% @see normalize/2
%% @see eval/2
-spec binstr_rule(input(), rule_param()) -> result().
binstr_rule(Input, _Param) ->
    do(
        [fun klsn_binstr:from_any/1]
      , Input
    ) .

%% @doc
%% Validate atoms used by validate/2, normalize/2, and eval/2.
%% Rule form: atom.
%% Accepts atoms; otherwise converts input via klsn_binstr:from_any/1 and
%% binary_to_existing_atom/1 (existing atoms only).
%%
%% Result:
%% - valid when Input is an atom.
%% - normalized when Input converts to an existing atom;
%%   reason is {invalid, atom, Input}.
%% - reject when Input cannot be converted to an existing atom;
%%   reason is {invalid, atom, Input}.
%%
%% Examples:
%% ```
%% 1> klsn_rule:validate(atom, ok).
%% 2> klsn_rule:normalize(atom, <<"ok">>).
%% 3> klsn_rule:eval(atom, <<"not_an_atom">>).
%% '''
%% @see validate/2
%% @see normalize/2
%% @see eval/2
-spec atom_rule(input(), rule_param()) -> result().
atom_rule(Input, _Param) ->
    do(
        fun is_atom/1
      , [fun(I) ->
            binary_to_existing_atom(klsn_binstr:from_any(I))
        end]
      , Input
    ) .

%% @doc
%% Validate enums used by validate/2, normalize/2, and eval/2.
%% Rule form: {enum, [atom()]}.
%% Matching compares klsn_binstr:from_any/1 for the input and each allowed enum.
%% Exact =:= matches are valid; otherwise the input normalizes to the matched enum.
%%
%% Result (eval/2):
%% - valid when Input matches an allowed enum exactly (Input =:= Enum).
%% - normalized when Input matches by binary conversion but is not =:=;
%%   reason is {invalid, enum, Input}.
%% - reject when no allowed enum matches; reason is {invalid_enum, AllowedEnums, Input}.
%% - reject when AllowedEnums is not a list; reason is {invalid, enum, Input}.
%%
%% normalize/2 returns the matched enum and discards the reason; use eval/2 to
%% inspect the normalization reason.
%%
%% Examples:
%% ```
%% 1> klsn_rule:eval({enum, [foo, bar]}, foo).
%% 2> klsn_rule:eval({enum, [foo, bar]}, <<"foo">>).
%% 3> klsn_rule:eval({enum, [foo, bar]}, baz).
%% 4> klsn_rule:normalize({enum, [foo, bar]}, <<"bar">>).
%% 5> klsn_rule:validate({enum, [foo, bar]}, baz).
%% '''
%% @see validate/2
%% @see normalize/2
%% @see eval/2
-spec enum_rule(input(), rule_param()) -> result().
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

%% @doc
%% Validate against a list of rules for validate/2, normalize/2, and eval/2.
%% Rule form: {any_of, [rule()]}.
%%
%% Result (eval/2):
%% - valid when any rule validates (short-circuit).
%% - normalized when no rule validates and at least one normalizes; output is
%%   from the first normalized rule; reason is {any_of, [reason()]}.
%% - reject when no rule validates or normalizes; reason is {any_of, [reason()]}.
%% - valid when the rule list is empty.
%%
%% Reasons are collected from each attempted rule (normalized or reject) in
%% rule order.
%%
%% normalize/2 returns the first normalized output when present; validate/2
%% accepts only the valid case and errors otherwise.
%%
%% Examples:
%% ```
%% 1> klsn_rule:eval({any_of, [integer, {exact, ok}]}, 3).
%% {valid,3}
%% 2> klsn_rule:eval({any_of, [integer, {exact, ok}]}, <<"10">>).
%% {normalized,10,{any_of,[{invalid,integer,<<"10">>},{invalid_exact,ok,<<"10">>}]}}
%% 3> klsn_rule:eval({any_of, [integer, {exact, ok}]}, <<"nope">>).
%% {reject,{any_of,[{invalid,integer,<<"nope">>},{invalid_exact,ok,<<"nope">>}]}}
%% 4> klsn_rule:normalize({any_of, [integer, {exact, ok}]}, <<"10">>).
%% 10
%% 5> klsn_rule:validate({any_of, [integer, {exact, ok}]}, 3).
%% ok
%% '''
%% @see validate/2
%% @see normalize/2
%% @see eval/2
-spec any_of_rule(input(), rule_param()) -> result().
any_of_rule(_Input, []) ->
    valid;
any_of_rule(Input, Rules) when is_list(Rules) ->
    any_of_rule_(Input, Rules, none, []);
any_of_rule(_, _) ->
    reject.

-spec any_of_rule_(input(), [rule()], klsn:optnl(output()), [reason()]) -> result().
any_of_rule_(_Input, [], {value, Output}, ReasonsRev) ->
    {normalized, Output, {any_of, lists:reverse(ReasonsRev)}};
any_of_rule_(_Input, [], none, ReasonsRev) ->
    {reject, {any_of, lists:reverse(ReasonsRev)}};
any_of_rule_(Input, [Rule|T], MaybeOutput0, ReasonsRev0) ->
    case eval(Rule, Input) of
        {valid, _} ->
            valid;
        {normalized, Output, Reason} ->
            MaybeOutput = case MaybeOutput0 of
                none ->
                    {value, Output};
                _ ->
                    MaybeOutput0
            end,
            any_of_rule_(Input, T, MaybeOutput, [Reason|ReasonsRev0]);
        {reject, Reason} ->
            any_of_rule_(Input, T, MaybeOutput0, [Reason|ReasonsRev0])
    end.

%% @doc
%% Apply all Rules; used by validate/2, normalize/2, and eval/2.
%%
%% Rule form: {all_of, [rule()]}.
%% Each rule is evaluated against the same Input in list order.
%%
%% Result (eval/2):
%% - valid when every rule returns valid (or Rules is empty).
%% - normalized when no rule rejects and at least one rule normalizes; output is
%%   the first valid output if present, otherwise the first normalized output;
%%   reason is {all_of, [reason()]} from the normalizing rules in list order.
%% - reject when any rule rejects; reason is {all_of, [reason()]} from the
%%   rejecting rules in list order (normalization reasons are discarded).
%%
%% normalize/2 returns the output and drops the reason; validate/2 errors on
%% normalized or reject results.
%%
%% Examples:
%% ```
%% 1> klsn_rule:eval({all_of, [integer, {range, {0, '=<', integer, '<', 10}}]}, 5).
%% {valid, 5}
%% 2> klsn_rule:eval({all_of, [integer, {range, {0, '=<', integer, '<', 10}}]}, <<"5">>).
%% {normalized, 5, {all_of, [{invalid, integer, <<"5">>}, {invalid, integer, <<"5">>}]}}
%% 3> klsn_rule:eval({all_of, [integer, {range, {0, '=<', integer, '<', 10}}]}, 20).
%% {reject, {all_of, [{invalid_range, {0, '=<', 20, '<', 10}}]}}
%% '''
%% @see validate/2
%% @see normalize/2
%% @see eval/2
-spec all_of_rule(input(), rule_param()) -> result().
all_of_rule(_Input, []) ->
    valid;
all_of_rule(Input, Rules) when is_list(Rules) ->
    {MaybeValidOutput, MaybeNormOutput, NormReasonsRev, RejectReasonsRev} =
        all_of_rule_(Input, Rules, none, none, [], []),
    case RejectReasonsRev of
        [] ->
            case NormReasonsRev of
                [] ->
                    valid;
                _ ->
                    Output = case MaybeValidOutput of
                        {value, ValidOutput} ->
                            ValidOutput;
                        none ->
                            {value, NormOutput} = MaybeNormOutput,
                            NormOutput
                    end,
                    {normalized, Output, {all_of, lists:reverse(NormReasonsRev)}}
            end;
        _ ->
            {reject, {all_of, lists:reverse(RejectReasonsRev)}}
    end;
all_of_rule(_, _) ->
    reject.

-spec all_of_rule_(
        input()
      , [rule()]
      , klsn:optnl(output())
      , klsn:optnl(output())
      , [reason()]
      , [reason()]
    ) -> {klsn:optnl(output()), klsn:optnl(output()), [reason()], [reason()]}.
all_of_rule_(_Input, [], MaybeValidOutput, MaybeNormOutput, NormReasonsRev, RejectReasonsRev) ->
    {MaybeValidOutput, MaybeNormOutput, NormReasonsRev, RejectReasonsRev};
all_of_rule_(Input, [Rule|T], MaybeValidOutput0, MaybeNormOutput0, NormReasonsRev0, RejectReasonsRev0) ->
    case eval(Rule, Input) of
        {valid, Output} ->
            MaybeValidOutput = case MaybeValidOutput0 of
                none ->
                    {value, Output};
                _ ->
                    MaybeValidOutput0
            end,
            all_of_rule_(Input, T, MaybeValidOutput, MaybeNormOutput0, NormReasonsRev0, RejectReasonsRev0);
        {normalized, Output, Reason} ->
            MaybeNormOutput = case MaybeNormOutput0 of
                none ->
                    {value, Output};
                _ ->
                    MaybeNormOutput0
            end,
            all_of_rule_(Input, T, MaybeValidOutput0, MaybeNormOutput, [Reason|NormReasonsRev0], RejectReasonsRev0);
        {reject, Reason} ->
            all_of_rule_(Input, T, MaybeValidOutput0, MaybeNormOutput0, NormReasonsRev0, [Reason|RejectReasonsRev0])
    end.

%% @doc
%% Apply rules left-to-right, feeding each output into the next rule.
%% Rule form: {foldl, [rule()]}.
%%
%% Result (eval/2):
%% - valid when every rule returns valid (output stays the original input).
%% - normalized when the first rule that normalizes succeeds; output is from the
%%   last rule and the reason is from the first normalization. Later
%%   normalizations keep the original reason.
%% - reject with the Reason from the first rule that rejects.
%%
%% normalize/2 returns the last output; validate/2 raises on normalize/reject.
%%
%% Examples:
%% ```
%% 1> klsn_rule:eval({foldl, [atom, {exact, ok}]}, ok).
%% {valid, ok}
%% 2> klsn_rule:eval({foldl, [atom, {exact, ok}]}, "ok").
%% {normalized, ok, {invalid, atom, "ok"}}
%% 3> klsn_rule:eval({foldl, [atom, {exact, nope}]}, "ok").
%% {reject, {invalid_exact, nope, ok}}
%% '''
%% @see validate/2
%% @see normalize/2
%% @see eval/2
-spec foldl_rule(input(), rule_param()) -> result().
foldl_rule(Input, Rules) when is_list(Rules) ->
    lists:foldl(fun(Rule, Acc) ->
        case Acc of
            {reject, _} ->
                Acc;
            {valid, Value} ->
                eval(Rule, Value);
            {normalized, Value, Reason} ->
                case eval(Rule, Value) of
                    {valid, Output} ->
                        {normalized, Output, Reason};
                    {normalized, Output, _Reason} ->
                        {normalized, Output, Reason};
                    {reject, RejectReason} ->
                        {reject, RejectReason}
                end
        end
    end, {valid, Input}, Rules);
foldl_rule(_, _) ->
    reject.

%% @doc
%% Optional rule wrapper; use as {optnl, Rule} in rule() with validate/2,
%% normalize/2, or eval/2. Output is klsn:optnl(Output)
%% ({value, Output} or none).
%%
%% Input handling:
%% - none is valid and stays none.
%% - {value, V} validates V with Rule.
%% - null | nil | undefined | false | [] | error normalize to none.
%% - {ok, V} | {true, V} | [V] | binary() | number() are treated as values,
%%   evaluated with Rule, and wrapped as {value, Output}.
%%
%% Result conditions (eval/2):
%% - valid when Input is none or {value, V} and Rule validates V.
%% - normalized when a wrapper/marker is converted or Rule normalizes V.
%% - reject when Input is unrecognized or Rule rejects V.
%%
%% Reasons (eval/2):
%% - {invalid, optnl, Input} when optnl_rule/2 normalizes without a rule reason
%%   (markers/wrappers that validate) or when Input is unrecognized.
%% - {invalid_optnl_value, Reason} when Rule normalizes or rejects V.
%%
%% validate/2 only accepts valid; normalize/2 returns the output and drops
%% the reason.
%%
%% Examples:
%% ```
%% 1> klsn_rule:eval({optnl, integer}, none).
%% {valid, none}
%% 2> klsn_rule:eval({optnl, integer}, null).
%% {normalized, none, {invalid, optnl, null}}
%% 3> klsn_rule:eval({optnl, integer}, {ok, 3}).
%% {normalized, {value, 3}, {invalid, optnl, {ok, 3}}}
%% 4> klsn_rule:eval({optnl, integer}, {value, 3}).
%% {valid, {value, 3}}
%% 5> klsn_rule:eval({optnl, integer}, {value, foo}).
%% {reject, {invalid_optnl_value, {invalid, integer, foo}}}
%% 6> klsn_rule:normalize({optnl, integer}, {ok, 3}).
%% {value, 3}
%% '''
%% @see validate/2
%% @see normalize/2
%% @see eval/2
-spec optnl_rule(input(), rule_param()) -> result().
optnl_rule(none, _Rule) ->
    valid;
optnl_rule({value, Value}, Rule) ->
    optnl_eval_value_(Value, Rule, valid);
optnl_rule(null, _Rule) ->
    {normalized, none};
optnl_rule(nil, _Rule) ->
    {normalized, none};
optnl_rule(undefined, _Rule) ->
    {normalized, none};
optnl_rule(false, _Rule) ->
    {normalized, none};
optnl_rule([], _Rule) ->
    {normalized, none};
optnl_rule(error, _Rule) ->
    {normalized, none};
optnl_rule({ok, Value}, Rule) ->
    optnl_eval_value_(Value, Rule, normalized);
optnl_rule({true, Value}, Rule) ->
    optnl_eval_value_(Value, Rule, normalized);
optnl_rule([Value], Rule) ->
    optnl_eval_value_(Value, Rule, normalized);
optnl_rule(Value, Rule) when is_binary(Value) ->
    optnl_eval_value_(Value, Rule, normalized);
optnl_rule(Value, Rule) when is_number(Value) ->
    optnl_eval_value_(Value, Rule, normalized);
optnl_rule(_, _Rule) ->
    reject.

%% @doc
%% Validate a nullable rule used by validate/2, normalize/2, and eval/2.
%% Rule form: {nullable, Rule} where Rule is a rule().
%%
%% Result (eval/2):
%% - valid when Input is null.
%% - normalized to null when Input is none.
%% - valid when Input is Value and Rule validates.
%% - normalized when Input is {value, Value} and Rule validates.
%% - normalized with {invalid_nullable_value, Reason} when Rule normalizes.
%% - reject with {invalid_nullable_value, Reason} when Rule rejects.
%%
%% Reasons (eval/2):
%% - {invalid, nullable, Input} when nullable_rule/2 normalizes without a rule
%%   reason (none or {value, Value} when Rule validates).
%% - {invalid_nullable_value, Reason} when Rule normalizes or rejects Value.
%%
%% Examples:
%% ```
%% 1> klsn_rule:eval({nullable, integer}, null).
%% 2> klsn_rule:eval({nullable, integer}, none).
%% 3> klsn_rule:eval({nullable, integer}, <<"12">>).
%% 4> klsn_rule:eval({nullable, integer}, {value, <<"12">>}).
%% '''
%% @see validate/2
%% @see normalize/2
%% @see eval/2
-spec nullable_rule(input(), rule_param()) -> result().
nullable_rule(null, _Rule) ->
    valid;
nullable_rule(none, _Rule) ->
    {normalized, null};
nullable_rule({value, Value}, Rule) ->
    nullable_eval_value_(Value, Rule, normalized);
nullable_rule(Value, Rule) ->
    nullable_eval_value_(Value, Rule, valid).

%% @doc
%% Enforce strict evaluation when used as {strict, Rule}.
%% Rule form: {strict, Rule} where Rule :: rule().
%%
%% Result (eval/2):
%% - valid when Rule returns {valid, Output}.
%% - normalized is never returned; when Rule normalizes with
%%   {normalized, Output, Reason}, this rule rejects with {strict, Reason}.
%% - reject when Rule rejects; the Reason passes through unchanged.
%%
%% validate/2 and normalize/2 only succeed on valid. When strict rejects,
%% they raise error({klsn_rule, Reason}) where Reason is the reject reason above.
%%
%% Examples:
%% ```
%% 1> klsn_rule:eval({strict, integer}, 42).
%% 2> klsn_rule:eval({strict, integer}, <<"42">>).
%% 3> klsn_rule:normalize({strict, integer}, 42).
%% '''
%% @see validate/2
%% @see normalize/2
%% @see eval/2
-spec strict_rule(input(), rule_param()) -> result().
strict_rule(Input, Rule) ->
    case eval(Rule, Input) of
        {valid, Output} ->
            {valid, Output};
        {normalized, _Output, Reason} ->
            {reject, {strict, Reason}};
        {reject, Reason} ->
            {reject, Reason}
    end.

-spec optnl_eval_value_(input(), rule(), valid | normalized) -> result().
optnl_eval_value_(Value, Rule, Validity) ->
    case eval(Rule, Value) of
        {valid, Output} ->
            case Validity of
                valid ->
                    {valid, {value, Output}};
                normalized ->
                    {normalized, {value, Output}}
            end;
        {normalized, Output, Reason} ->
            {normalized, {value, Output}, {invalid_optnl_value, Reason}};
        {reject, Reason} ->
            {reject, {invalid_optnl_value, Reason}}
    end.

-spec nullable_eval_value_(input(), rule(), valid | normalized) -> result().
nullable_eval_value_(Value, Rule, Validity) ->
    case eval(Rule, Value) of
        {valid, Output} ->
            case Validity of
                valid ->
                    {valid, Output};
                normalized ->
                    {normalized, Output}
            end;
        {normalized, Output, Reason} ->
            {normalized, Output, {invalid_nullable_value, Reason}};
        {reject, Reason} ->
            {reject, {invalid_nullable_value, Reason}}
    end.

%% @doc
%% Validate list inputs by applying an element rule to each entry.
%%
%% Rule form: `{list, ElementRule}' where `ElementRule :: rule()'.
%%
%% Result (eval/2):
%% - valid when Input is a list and every element validates (no normalization).
%% - normalized when Input is a list, no element rejects, and the first
%%   normalized element is at Index (1-based); output is a list of element
%%   outputs; reason is {invalid_list_element, Index, Reason}.
%% - reject when Input is not a list; reason is {invalid, list, Input}.
%% - reject when the first rejecting element is at Index; reason is
%%   {invalid_list_element, Index, Reason}.
%%
%% normalize/2 returns the output list on valid/normalized and raises on reject.
%% validate/2 returns ok only on valid; it raises on normalized/reject.
%%
%% Examples:
%% ```
%% 1> klsn_rule:eval({list, integer}, [1, 2]).
%% {valid, [1, 2]}
%% 2> klsn_rule:eval({list, integer}, [<<"1">>, <<"2">>]).
%% {normalized, [1, 2], {invalid_list_element, 1, {invalid, integer, <<"1">>}}}
%% 3> klsn_rule:eval({list, integer}, [1, <<"bad">>]).
%% {reject, {invalid_list_element, 2, {invalid, integer, <<"bad">>}}}
%% 4> klsn_rule:eval({list, integer}, <<"1">>).
%% {reject, {invalid, list, <<"1">>}}
%% 5> klsn_rule:normalize({list, integer}, [<<"1">>]).
%% [1]
%% 6> klsn_rule:validate({list, integer}, [<<"1">>]).
%% ** exception error: {klsn_rule,{invalid_list_element,1,{invalid,integer,<<"1">>}}}
%% '''
%% @see validate/2
%% @see normalize/2
%% @see eval/2
-spec list_rule(input(), rule_param()) -> result().
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

%% @doc
%% Validate tuple inputs against element rules.
%%
%% Rule form: {tuple, Rules} where Rules is a list or tuple of rule().
%%
%% Result (eval/2):
%% - valid when Input is a tuple, arity matches Rules, and all elements validate.
%% - normalized when Input is a tuple, no element rejects, and the first
%%   normalized element is at Index; output is a tuple of element outputs; reason
%%   is {invalid_tuple_element, Index, Reason}.
%% - reject when Input is not a tuple; reason is {invalid, tuple, Input}.
%% - reject when arity mismatches; reason is {invalid_tuple_size, Expected, Input}.
%% - reject when the first element to reject is at Index; reason is
%%   {invalid_tuple_element, Index, Reason}.
%%
%% normalize/2 returns the tuple output on valid/normalized and raises on reject.
%% validate/2 returns ok only for valid results (normalized counts as invalid).
%%
%% Examples:
%% ```
%% 1> klsn_rule:eval({tuple, [integer, atom]}, {1, ok}).
%% {valid, {1, ok}}
%% 2> klsn_rule:eval({tuple, [integer, atom]}, {"1", ok}).
%% {normalized, {1, ok}, {invalid_tuple_element, 1, {invalid, integer, "1"}}}
%% 3> klsn_rule:eval({tuple, {integer, atom}}, {1, ok}).
%% {valid, {1, ok}}
%% 4> klsn_rule:eval({tuple, [integer, atom]}, {1}).
%% {reject, {invalid_tuple_size, 2, {1}}}
%% 5> klsn_rule:eval({tuple, [integer, atom]}, [1, ok]).
%% {reject, {invalid, tuple, [1, ok]}}
%% 6> klsn_rule:normalize({tuple, [integer, atom]}, {ok, ok}).
%% exception error: {klsn_rule,{invalid_tuple_element,1,{invalid,integer,ok}}}
%% '''
%% @see validate/2
%% @see normalize/2
%% @see eval/2
-spec tuple_rule(input(), rule_param()) -> result().
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

%% @doc
%% Validate map input for validate/2, normalize/2, and eval/2.
%% Rule form: {map, {KeyRule, ValueRule}} where KeyRule and ValueRule are rule().
%%
%% Result (eval/2):
%% - valid when Input is a map and all keys/values validate without normalization.
%% - normalized when Input is a map, no key/value rejects, and at least one
%%   key or value normalizes; output is a map with normalized keys/values.
%% - reject when Input is not a map, when a key or value rejects, when
%%   normalized keys collide, or when the rule parameter is not {KeyRule, ValueRule}.
%%
%% Reason (eval/2):
%% - {invalid, map, Input} when Input is not a map or the rule parameter is invalid.
%% - {invalid_map_key, Reason} when a key rejects or normalizes.
%% - {invalid_map_value, Key, Reason} when a value rejects or normalizes
%%   (Key is the original input key).
%% - {map_key_conflict, Key} when key normalization produces duplicates.
%%
%% normalize/2 returns the (possibly normalized) map and drops the reason.
%% validate/2 returns ok only for valid results; it raises on normalized or reject.
%%
%% Examples:
%% ```
%% 1> klsn_rule:eval({map, {integer, integer}}, #{1 => 2}).
%% {valid, #{1 => 2}}
%% 2> klsn_rule:eval({map, {integer, integer}}, #{1 => "2"}).
%% {normalized, #{1 => 2}, {invalid_map_value, 1, {invalid, integer, "2"}}}
%% 3> klsn_rule:eval({map, {integer, integer}}, #{"nope" => 1}).
%% {reject, {invalid_map_key, {invalid, integer, "nope"}}}
%% 4> klsn_rule:eval({map, {integer, term}}, #{"1" => a, 1 => b}).
%% {reject, {map_key_conflict, 1}}
%% 5> klsn_rule:eval({map, {integer, integer}}, [1, 2]).
%% {reject, {invalid, map, [1, 2]}}
%% 6> klsn_rule:normalize({map, {integer, integer}}, #{"1" => "2"}).
%% #{1 => 2}
%% '''
%% @see validate/2
%% @see normalize/2
%% @see eval/2
-spec map_rule(input(), rule_param()) -> result().
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

%% @doc
%% Validate struct maps against a field spec for use with validate/2, normalize/2,
%% and eval/2.
%%
%% Rule form: {struct, #{Field => {required | optional, rule()}}}.
%% Field must be an atom; Input must be a map. Keys are matched by comparing
%% klsn_binstr:from_any(Key) and klsn_binstr:from_any(Field), so Field,
%% `<<"field">>', and "field" are treated as the same field.
%%
%% Output normalization:
%% - extra keys are dropped,
%% - matched keys are normalized to their atom field names,
%% - field values are normalized when their rule normalizes.
%%
%% When evaluated via {@link eval/2}:
%% - valid when all required fields are present, no extra or duplicate keys
%%   exist, and all field rules return valid.
%% - normalized when extra keys are removed, keys are normalized, or any field
%%   value normalizes. Reasons are:
%%   {invalid_struct_field, Key} for extra keys or key normalization, and
%%   {invalid_struct_value, Field, Reason} for value normalization.
%% When multiple normalization causes exist, the reason reports extra keys
%% first, then key normalization, then value normalization.
%% - reject when required fields are missing, a field has duplicate keys, or a
%%   field rule rejects. Reasons are:
%%   {missing_required_field, Field},
%%   {struct_field_conflict, Field},
%%   {invalid_struct_value, Field, Reason}.
%% - reject with {invalid, struct, Input} when Input or the spec is malformed.
%%
%% normalize/2 returns the output map and drops the reason.
%% validate/2 raises error({klsn_rule, Reason}) on normalized or reject.
%%
%% Examples:
%% ```
%% 1> Rule = {struct, #{name => {required, binstr}, age => {optional, integer}}}.
%% 2> klsn_rule:eval(Rule, #{name => <<"Ada">>, age => 32}).
%% {valid, #{name => <<"Ada">>, age => 32}}
%% 3> klsn_rule:eval(Rule, #{<<"name">> => <<"Ada">>, age => <<"42">>}).
%% {normalized, #{name => <<"Ada">>, age => 42},
%%  {invalid_struct_field, <<"name">>}}
%% 4> klsn_rule:normalize(Rule, #{<<"name">> => <<"Ada">>, extra => 1}).
%% #{name => <<"Ada">>}
%% 5> klsn_rule:eval(Rule, #{age => 32}).
%% {reject, {missing_required_field, name}}
%% 6> klsn_rule:eval(Rule, #{name => <<"Ada">>, <<"name">> => <<"Ada">>}).
%% {reject, {struct_field_conflict, name}}
%% '''
%% @see validate/2
%% @see normalize/2
%% @see eval/2
-spec struct_rule(input(), rule_param()) -> result().
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


%% @doc
%% Evaluate a rule against an input and return a strict result.
%%
%% Builtin rules may be passed as atoms (for example, `integer') or
%% tuples; they are dispatched to `name_rule/2' functions. Unknown
%% rules return `{reject, {unknown_rule, Rule}}'.
%%
%% Custom rules use `{custom, Name, Fun, Param}' where
%% `Fun(Input, Param) -> result()'. Return handling:
%% - `valid' or `{valid, Input}' maps to `{valid, Input}'
%% - `{valid, Output}' with `Output =/= Input' raises
%%   `error({invalid_custom_rule, ...})'
%% - `{normalized, Output}' maps to
%%   `{normalized, Output, {invalid, Name, Input}}'
%% - `reject' maps to `{reject, {invalid, Name, Input}}'
%% - `{normalized, Output, Reason}' and `{reject, Reason}'
%%   pass through unchanged
%%
%% Custom reasons are expected to use `{custom, term()}' so they do
%% not conflict with reasons from builtin rules.
%%
%% Examples:
%% ```
%% 1> klsn_rule:eval(integer, <<"10">>).
%% {normalized,10,{invalid,integer,<<"10">>}}
%% 2> Unwrap = fun({ok, V}, _Param) ->
%%        {normalized, V, {custom, unwrapped}};
%%    (_, _Param) ->
%%        {reject, {custom, unexpected}}
%% end.
%% 3> Rule = {custom, unwrap_ok, Unwrap, []}.
%% 4> klsn_rule:eval(Rule, {ok, 5}).
%% {normalized,5,{custom,unwrapped}}
%% 5> klsn_rule:eval(Rule, error).
%% {reject,{custom,unexpected}}
%% '''
%% @see validate/2
%% @see normalize/2
-spec eval(rule(), input()) -> strict_result().
eval({custom, Name, Custom, Param}=Arg1, Input) ->
    case Custom(Input, Param) of
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


-spec rule_to_custom_(rule()) -> klsn:optnl({custom, name(), custom(), rule_param()}).
rule_to_custom_({custom, Name, Custom, Param}) ->
    {value, {custom, Name, Custom, Param}};
rule_to_custom_(Rule) ->
    {Name, Param} = case Rule of
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
            {value, {custom, Name, fun ?MODULE:Function/Arity, Param}};
        _ ->
            none
    end.
