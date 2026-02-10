-module(klsn_rule).

%% public functions
-export([
        validate/2
      , normalize/2
      , eval/3
      , lookup_alias/1
    ]).

%% builtin rules
-export([
        term_rule/3
      , exact_rule/3
      , default_rule/3
      , boolean_rule/3
      , integer_rule/3
      , float_rule/3
      , number_rule/3
      , range_rule/3
      , alias_rule/3
      , with_defs_rule/3
      , ref_rule/3
      , timeout_rule/3
      , binstr_rule/3
      , atom_rule/3
      , enum_rule/3
      , any_of_rule/3
      , all_of_rule/3
      , foldl_rule/3
      , optnl_rule/3
      , nullable_rule/3
      , strict_rule/3
      , list_rule/3
      , tuple_rule/3
      , map_rule/3
      , struct_rule/3
    ]).

-export_type([
        name/0
      , input/0
      , output/0
      , state/0
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

-type state() :: #{
        klsn_rule => state_()
      , module() => term()
    }.
-type state_() :: #{
        definitions => definitions()
    }.

-type ref_key() :: klsn:binstr().

-type definitions() :: #{ref_key() => rule()}.

-type alias() :: atom().

%% Just to define a type by using an rule alias.
-type alias(_AliasName) :: term().
-type alias(_Module, _AliasName) :: term().

-type alias_ref() :: {module(), alias()}.

-type custom() :: fun( (input(), rule_param(), state()) -> result() ).

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
              | {with_defs, {definitions(), rule()}}
              | {ref, ref_key()}
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
                | {invalid_ref, ref_key(), reason()}
                | {undefined_ref, ref_key(), input()}
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
%% This calls {@link eval/3} and only accepts a `{valid, Output}'
%% result. Any normalized or reject result raises
%% `error({klsn_rule, Reason})'.
%%
%% Examples:
%% ```
%% 1> klsn_rule:validate(10, integer).
%% ok
%% 2> klsn_rule:validate(<<"10">>, integer).
%% ** exception error: {klsn_rule,{invalid,integer,<<"10">>}}
%% '''
%% @see normalize/2
%% @see eval/3
-spec validate(input(), rule()) -> ok.
validate(Input, Rule) ->
    case eval(Input, Rule, #{}) of
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
%% This calls {@link eval/3}. Valid and normalized results return the output,
%% while reject results raise `error({klsn_rule, Reason})'.
%% Normalization reasons are dropped; use {@link eval/3} if you need them.
%%
%% Examples:
%% ```
%% 1> klsn_rule:normalize(<<"10">>, integer).
%% 10
%% 2> klsn_rule:normalize(5, {range, {0, '=<', integer, '<', 10}}).
%% 5
%% 3> klsn_rule:normalize(99, {range, {0, '=<', integer, '<', 10}}).
%% ** exception error: {klsn_rule,{invalid_range,{0,'=<',99,'<',10}}}
%% '''
%% @see validate/2
%% @see eval/3
-spec normalize(input(), rule()) -> output().
normalize(Input, Rule) ->
    case eval(Input, Rule, #{}) of
        {valid, Output} ->
            Output;
        {normalized, Output, _Reason} ->
            Output;
        {reject, Reason} ->
            error({?MODULE, Reason})
    end.

%% @doc
%% Accept any input and always return valid for validate/2, normalize/2, and eval/3.
%% Rule form: term (rule()).
%%
%% Result (eval/3):
%% - valid with the input unchanged.
%% - never normalized (no reason()).
%% - never rejected (no reason()).
%%
%% Examples:
%% ```
%% 1> klsn_rule:validate({any, value}, term).
%% ok
%% 2> klsn_rule:normalize(123, term).
%% 123
%% 3> klsn_rule:eval([a, b], term, #{}).
%% {valid, [a, b]}
%% '''
%% @see validate/2
%% @see normalize/2
%% @see eval/3
-spec term_rule(input(), rule_param(), state()) -> result().
term_rule(_Input, _Param, _State) ->
    valid.

%% @doc
%% Match an input against the exact rule parameter value.
%%
%% Rule form: {exact, Exact}.
%%
%% When evaluated via {@link eval/3}:
%% - valid: {valid, Input} when Input =:= Exact
%% - reject: {reject, {invalid_exact, Exact, Input}}
%%
%% This rule never produces a normalized result. {@link normalize/2} either
%% returns the original input or raises with {klsn_rule, {invalid_exact, Exact, Input}}.
%%
%% Examples:
%% ```
%% 1> klsn_rule:eval(42, {exact, 42}, #{}).
%% {valid, 42}
%% 2> klsn_rule:eval(7, {exact, 42}, #{}).
%% {reject, {invalid_exact, 42, 7}}
%% 3> klsn_rule:normalize(ok, {exact, ok}).
%% ok
%% 4> klsn_rule:normalize(error, {exact, ok}).
%% ** exception error: {klsn_rule,{invalid_exact,ok,error}}
%% 5> klsn_rule:validate(ok, {exact, ok}).
%% ok
%% '''
%% @see validate/2
%% @see normalize/2
%% @see eval/3
-spec exact_rule(input(), rule_param(), state()) -> result().
exact_rule(Input, Exact, _State) ->
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
%% When evaluated via {@link eval/3}:
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
%% - If Rule normalizes without a reason, {@link eval/3} uses
%%   {invalid, RuleName, Input} where RuleName is the inner rule name.
%% - {@link normalize/2} returns Output or Default and drops the reason.
%% - {@link validate/2} raises error({klsn_rule, Reason}) on normalized or
%%   reject results.
%%
%% Examples:
%% ```
%% 1> klsn_rule:eval(ok, {default, {ok, {exact, ok}}}, #{}).
%% {valid, ok}
%% 2> klsn_rule:eval(error, {default, {ok, {exact, ok}}}, #{}).
%% {normalized, ok, {invalid_exact, ok, error}}
%% 3> klsn_rule:normalize(1, {default, {0, {exact, 0}}}).
%% 0
%% 4> klsn_rule:validate(1, {default, {0, {exact, 0}}}).
%% ** exception error: {klsn_rule,{invalid_exact,0,1}}
%% 5> klsn_rule:eval(1, {default, []}, #{}).
%% {reject, {invalid, default, 1}}
%% '''
%% @see validate/2
%% @see normalize/2
%% @see eval/3
-spec default_rule(input(), rule_param(), state()) -> result().
default_rule(Input, {Default, Rule}, State) ->
    case eval(Input, Rule, State) of
        {valid, Output} ->
            {valid, Output};
        {normalized, Output, Reason} ->
            {normalized, Output, Reason};
        {reject, Reason} ->
            {normalized, Default, Reason}
    end;
default_rule(_, _, _State) ->
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
%% When evaluated via {@link eval/3}:
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
%% 1> klsn_rule:eval(true, boolean, #{}).
%% {valid, true}
%% 2> klsn_rule:eval(<<"false">>, boolean, #{}).
%% {normalized, false, {invalid, boolean, <<"false">>}}
%% 3> klsn_rule:normalize(0, boolean).
%% false
%% 4> klsn_rule:eval(<<"0">>, boolean, #{}).
%% {normalized, true, {invalid, boolean, <<"0">>}}
%% 5> klsn_rule:eval(#{}, boolean, #{}).
%% {reject, {invalid, boolean, #{}}}
%% 6> klsn_rule:validate(<<"false">>, boolean).
%% ** exception error: {klsn_rule,{invalid,boolean,<<"false">>}}
%% '''
%% @see validate/2
%% @see normalize/2
%% @see eval/3
-spec boolean_rule(input(), rule_param(), state()) -> result().
boolean_rule(Input, _Param, _State) ->
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
%% Validate integer input for use with validate/2, normalize/2, and eval/3.
%% Rule form: integer.
%%
%% Result (eval/3):
%% - valid when Input is an integer.
%% - normalized when Input is a list or binary that parses via
%%   binary_to_integer/1 or list_to_integer/1.
%% - reject when Input is neither an integer nor a parseable list/binary.
%%
%% Reason (eval/3):
%% - {invalid, integer, Input} on normalize and reject.
%%   validate/2 raises error({klsn_rule, {invalid, integer, Input}}) when
%%   normalization or rejection occurs.
%%
%% Examples:
%% ```
%% 1> klsn_rule:eval(10, integer, #{}).
%% 2> klsn_rule:eval("10", integer, #{}).
%% 3> klsn_rule:normalize("10", integer).
%% 4> klsn_rule:eval("nope", integer, #{}).
%% '''
%% @see validate/2
%% @see normalize/2
%% @see eval/3
-spec integer_rule(input(), rule_param(), state()) -> result().
integer_rule(Input, _Param, _State) ->
    do(
        fun is_integer/1
      , [fun binary_to_integer/1, fun list_to_integer/1]
      , Input
    ) .

%% @doc
%% Validate float input for validate/2, normalize/2, and eval/3.
%% Rule form: float.
%%
%% Result (eval/3):
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
%% 1> klsn_rule:eval(1.25, float, #{}).
%% {valid, 1.25}
%% 2> klsn_rule:eval(<<"1.25">>, float, #{}).
%% {normalized, 1.25, {invalid, float, <<"1.25">>}}
%% 3> klsn_rule:normalize("1.25", float).
%% 1.25
%% 4> klsn_rule:eval(<<"nope">>, float, #{}).
%% {reject, {invalid, float, <<"nope">>}}
%% '''
%% @see validate/2
%% @see normalize/2
%% @see eval/3
-spec float_rule(input(), rule_param(), state()) -> result().
float_rule(Input, _Param, _State) ->
    do(
        fun is_float/1
      , [fun binary_to_float/1, fun list_to_float/1]
      , Input
    ) .

%% @doc
%% Validate numeric input for validate/2, normalize/2, and eval/3.
%% Rule form: number (or {number, Param}; Param is ignored).
%%
%% Result (eval/3):
%% - valid when Input is a number (integer or float).
%% - normalized when Input is a binary or list that parses as an integer/float;
%%   parsing order is binary_to_integer/1, binary_to_float/1,
%%   list_to_integer/1, list_to_float/1.
%% - reject when Input is not numeric and cannot be parsed.
%%
%% Reason (eval/3):
%% - {invalid, number, Input} on normalize or reject.
%%
%% normalize/2 returns the parsed number (or original) or raises with
%% {klsn_rule, {invalid, number, Input}}. validate/2 returns ok only for
%% already-numeric input and raises for normalized/reject results.
%%
%% Examples:
%% ```
%% 1> klsn_rule:eval(10, number, #{}).
%% {valid, 10}
%% 2> klsn_rule:eval(<<"10">>, number, #{}).
%% {normalized, 10, {invalid, number, <<"10">>}}
%% 3> klsn_rule:normalize("1.5", number).
%% 1.5
%% 4> klsn_rule:eval(<<"nope">>, number, #{}).
%% {reject, {invalid, number, <<"nope">>}}
%% '''
%% @see validate/2
%% @see normalize/2
%% @see eval/3
-spec number_rule(input(), rule_param(), state()) -> result().
number_rule(Input, _Param, _State) ->
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
%% Validate a range rule used by validate/2, normalize/2, and eval/3.
%% Rule form: {range, range_(Rule)} where Rule is rule() and range_(Rule)
%% is one of:
%%   {Rule, Op, Upper} | {Lower, Op, Rule} | {Lower, Op1, Rule, Op2, Upper}
%% Op/Op1/Op2 are `` '<' '' or `` '=<' ''. Lower/Upper are numbers.
%%
%% Result (eval/3):
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
%% 1> klsn_rule:eval(<<"10">>, {range, {integer, '=<', 10}}, #{}).
%% 2> klsn_rule:eval(5, {range, {0, '=<', integer, '<', 10}}, #{}).
%% '''
%% @see validate/2
%% @see normalize/2
%% @see eval/3
-spec range_rule(input(), rule_param(), state()) -> result().
range_rule(Input, {Subject, Op, Upper}, State)
    when (Op =:= '<' orelse Op =:= '=<'),
         is_number(Upper),
         is_number(Subject) =:= false ->
    case eval(Input, Subject, State) of
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
range_rule(Input, {Lower, Op, Subject}, State)
    when (Op =:= '<' orelse Op =:= '=<'),
         is_number(Lower),
         is_number(Subject) =:= false ->
    case eval(Input, Subject, State) of
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
range_rule(Input, {Lower, Op1, Subject, Op2, Upper}, State)
    when (Op1 =:= '<' orelse Op1 =:= '=<'),
         (Op2 =:= '<' orelse Op2 =:= '=<'),
         is_number(Lower),
         is_number(Upper),
         is_number(Subject) =:= false ->
    case eval(Input, Subject, State) of
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
range_rule(_, _, _State) ->
    reject.

%% @doc
%% Resolve and evaluate a named rule alias declared via -klsn_rule_alias.
%%
%% Rule form: {alias, AliasRef} where AliasRef is {Module, Alias}.
%%
%% Result (eval/3):
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
%% 2> klsn_rule:eval(42, {alias, MyAlias}, #{}).
%% {valid, 42}
%% 3> klsn_rule:eval(<<"42">>, {alias, MyAlias}, #{}).
%% {normalized, 42, {invalid_alias, MyAlias, {invalid, integer, <<"42">>}}}
%% '''
%% @see lookup_alias/1
%% @see eval/3
-spec alias_rule(input(), rule_param(), state()) -> result().
alias_rule(Input, {Module, Alias}=AliasRef, State)
    when is_atom(Module), is_atom(Alias) ->
    alias_rule_eval_(Input, AliasRef, State);
alias_rule(_, _, _State) ->
    reject.

-spec alias_rule_eval_(input(), alias_ref(), state()) -> result().
alias_rule_eval_(Input, AliasRef, State) ->
    case lookup_alias(AliasRef) of
        {value, Rule} ->
            case eval(Input, Rule, State) of
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
%% Evaluate a rule with a definitions map stored in the evaluation state.
%%
%% Rule form: {with_defs, {Definitions, Rule}} where Definitions is a map of
%% reference name to rule().
%%
%% Result (eval/3):
%% - valid/normalized/reject according to Rule evaluated with the updated state.
%%
%% Examples:
%% ```
%% 1> Defs = #{<<"n">> => integer}.
%% 2> klsn_rule:eval(1, {with_defs, {Defs, {ref, <<"n">>}}}, #{}).
%% {valid, 1}
%% '''
%% @see ref_rule/3
%% @see eval/3
-spec with_defs_rule(input(), rule_param(), state()) -> result().
with_defs_rule(Input, {Defs, Rule}, State) when is_map(Defs) ->
    Defs0 = klsn_map:get([?MODULE, definitions], State, #{}),
    State1 = klsn_map:upsert([?MODULE, definitions], maps:merge(Defs0, Defs), State),
    eval(Input, Rule, State1);
with_defs_rule(_, _, _State) ->
    reject.

%% @doc
%% Resolve and evaluate a rule from the current definition set.
%%
%% Rule form: {ref, RefName} where RefName is a binary name that maps to a rule
%% stored by {@link with_defs_rule/3}.
%%
%% Result (eval/3):
%% - valid when the referenced rule validates.
%% - normalized when the referenced rule normalizes; reason is
%%   {invalid_ref, RefName, Reason}.
%% - reject when the referenced rule rejects; reason is
%%   {invalid_ref, RefName, Reason}.
%% - reject with {undefined_ref, RefName, Input} when missing.
%%
%% Examples:
%% ```
%% 1> Defs = #{<<"n">> => integer}.
%% 2> klsn_rule:eval(<<"1">>, {with_defs, {Defs, {ref, <<"n">>}}}, #{}).
%% {normalized, 1, {invalid_ref, <<"n">>, {invalid, integer, <<"1">>}}}
%% '''
%% @see with_defs_rule/3
%% @see eval/3
-spec ref_rule(input(), rule_param(), state()) -> result().
ref_rule(Input, Ref, State) ->
    case klsn_map:lookup([?MODULE, definitions, Ref], State) of
        {value, Rule} ->
            case eval(Input, Rule, State) of
                {valid, Output} ->
                    {valid, Output};
                {normalized, Output, Reason} ->
                    {normalized, Output, {invalid_ref, Ref, Reason}};
                {reject, Reason} ->
                    {reject, {invalid_ref, Ref, Reason}}
            end;
        none ->
            {reject, {undefined_ref, Ref, Input}}
    end.

%% @doc
%% Validate timeout input for validate/2, normalize/2, and eval/3.
%% Rule form: timeout.
%%
%% Result (eval/3):
%% - valid when Input is infinity or a non-negative integer.
%% - normalized when Input is a list/binary that parses via
%%   binary_to_integer/1 or list_to_integer/1, or the string "infinity".
%% - reject when Input cannot be converted.
%%
%% Reason (eval/3):
%% - {invalid, timeout, Input} on normalize or reject.
%%
%% normalize/2 returns the converted timeout (or original) or raises with
%% {klsn_rule, {invalid, timeout, Input}}. validate/2 returns ok only when the
%% input is already valid and raises on normalization or reject.
%%
%% Examples:
%% ```
%% 1> klsn_rule:eval(infinity, timeout, #{}).
%% 2> klsn_rule:eval(0, timeout, #{}).
%% 3> klsn_rule:eval(<<"15">>, timeout, #{}).
%% 4> klsn_rule:normalize("15", timeout).
%% 5> klsn_rule:eval(foo, timeout, #{}).
%% '''
%% @see validate/2
%% @see normalize/2
%% @see eval/3
-spec timeout_rule(input(), rule_param(), state()) -> result().
timeout_rule(Input, _Param, State) ->
    Rule = {any_of, [{enum, [infinity]}, {range, {0, '=<', integer}}]},
    case eval(Input, Rule, State) of
        {valid, _Output} ->
            valid;
        {normalized, Output, _Reason} ->
            {normalized, Output};
        {reject, _Reason} ->
            reject
    end.

%% @doc
%% Validate binary strings used by validate/2, normalize/2, and eval/3.
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
%% 1> klsn_rule:eval(<<"ok">>, binstr, #{}).
%% 2> klsn_rule:eval(42, binstr, #{}).
%% 3> klsn_rule:normalize([<<"a">>, "b"], binstr).
%% 4> klsn_rule:eval(#{}, binstr, #{}).
%% '''
%% @see validate/2
%% @see normalize/2
%% @see eval/3
-spec binstr_rule(input(), rule_param(), state()) -> result().
binstr_rule(Input, _Param, _State) ->
    do(
        [fun klsn_binstr:from_any/1]
      , Input
    ) .

%% @doc
%% Validate atoms used by validate/2, normalize/2, and eval/3.
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
%% 1> klsn_rule:validate(ok, atom).
%% 2> klsn_rule:normalize(<<"ok">>, atom).
%% 3> klsn_rule:eval(<<"not_an_atom">>, atom, #{}).
%% '''
%% @see validate/2
%% @see normalize/2
%% @see eval/3
-spec atom_rule(input(), rule_param(), state()) -> result().
atom_rule(Input, _Param, _State) ->
    do(
        fun is_atom/1
      , [fun(I) ->
            binary_to_existing_atom(klsn_binstr:from_any(I))
        end]
      , Input
    ) .

%% @doc
%% Validate enums used by validate/2, normalize/2, and eval/3.
%% Rule form: {enum, [atom()]}.
%% Matching compares klsn_binstr:from_any/1 for the input and each allowed enum.
%% Exact =:= matches are valid; otherwise the input normalizes to the matched enum.
%%
%% Result (eval/3):
%% - valid when Input matches an allowed enum exactly (Input =:= Enum).
%% - normalized when Input matches by binary conversion but is not =:=;
%%   reason is {invalid, enum, Input}.
%% - reject when no allowed enum matches; reason is {invalid_enum, AllowedEnums, Input}.
%% - reject when AllowedEnums is not a list; reason is {invalid, enum, Input}.
%%
%% normalize/2 returns the matched enum and discards the reason; use eval/3 to
%% inspect the normalization reason.
%%
%% Examples:
%% ```
%% 1> klsn_rule:eval(foo, {enum, [foo, bar]}, #{}).
%% 2> klsn_rule:eval(<<"foo">>, {enum, [foo, bar]}, #{}).
%% 3> klsn_rule:eval(baz, {enum, [foo, bar]}, #{}).
%% 4> klsn_rule:normalize(<<"bar">>, {enum, [foo, bar]}).
%% 5> klsn_rule:validate(baz, {enum, [foo, bar]}).
%% '''
%% @see validate/2
%% @see normalize/2
%% @see eval/3
-spec enum_rule(input(), rule_param(), state()) -> result().
enum_rule(Input, AllowedEnums, _State) when is_list(AllowedEnums) ->
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
enum_rule(_, _, _State) ->
    reject.

%% @doc
%% Validate against a list of rules for validate/2, normalize/2, and eval/3.
%% Rule form: {any_of, [rule()]}.
%%
%% Result (eval/3):
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
%% 1> klsn_rule:eval(3, {any_of, [integer, {exact, ok}]}, #{}).
%% {valid,3}
%% 2> klsn_rule:eval(<<"10">>, {any_of, [integer, {exact, ok}]}, #{}).
%% {normalized,10,{any_of,[{invalid,integer,<<"10">>},{invalid_exact,ok,<<"10">>}]}}
%% 3> klsn_rule:eval(<<"nope">>, {any_of, [integer, {exact, ok}]}, #{}).
%% {reject,{any_of,[{invalid,integer,<<"nope">>},{invalid_exact,ok,<<"nope">>}]}}
%% 4> klsn_rule:normalize(<<"10">>, {any_of, [integer, {exact, ok}]}).
%% 10
%% 5> klsn_rule:validate(3, {any_of, [integer, {exact, ok}]}).
%% ok
%% '''
%% @see validate/2
%% @see normalize/2
%% @see eval/3
-spec any_of_rule(input(), rule_param(), state()) -> result().
any_of_rule(_Input, [], _State) ->
    valid;
any_of_rule(Input, Rules, State) when is_list(Rules) ->
    any_of_rule_(Input, Rules, none, [], State);
any_of_rule(_, _, _State) ->
    reject.

-spec any_of_rule_(input(), [rule()], klsn:optnl(output()), [reason()], state()) -> result().
any_of_rule_(_Input, [], {value, Output}, ReasonsRev, _State) ->
    {normalized, Output, {any_of, lists:reverse(ReasonsRev)}};
any_of_rule_(_Input, [], none, ReasonsRev, _State) ->
    {reject, {any_of, lists:reverse(ReasonsRev)}};
any_of_rule_(Input, [Rule|T], MaybeOutput0, ReasonsRev0, State) ->
    case eval(Input, Rule, State) of
        {valid, _} ->
            valid;
        {normalized, Output, Reason} ->
            MaybeOutput = case MaybeOutput0 of
                none ->
                    {value, Output};
                _ ->
                    MaybeOutput0
            end,
            any_of_rule_(Input, T, MaybeOutput, [Reason|ReasonsRev0], State);
        {reject, Reason} ->
            any_of_rule_(Input, T, MaybeOutput0, [Reason|ReasonsRev0], State)
    end.

%% @doc
%% Apply all Rules; used by validate/2, normalize/2, and eval/3.
%%
%% Rule form: {all_of, [rule()]}.
%% Each rule is evaluated against the same Input in list order.
%%
%% Result (eval/3):
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
%% 1> klsn_rule:eval(5, {all_of, [integer, {range, {0, '=<', integer, '<', 10}}]}, #{}).
%% {valid, 5}
%% 2> klsn_rule:eval(<<"5">>, {all_of, [integer, {range, {0, '=<', integer, '<', 10}}]}, #{}).
%% {normalized, 5, {all_of, [{invalid, integer, <<"5">>}, {invalid, integer, <<"5">>}]}}
%% 3> klsn_rule:eval(20, {all_of, [integer, {range, {0, '=<', integer, '<', 10}}]}, #{}).
%% {reject, {all_of, [{invalid_range, {0, '=<', 20, '<', 10}}]}}
%% '''
%% @see validate/2
%% @see normalize/2
%% @see eval/3
-spec all_of_rule(input(), rule_param(), state()) -> result().
all_of_rule(_Input, [], _State) ->
    valid;
all_of_rule(Input, Rules, State) when is_list(Rules) ->
    {MaybeValidOutput, MaybeNormOutput, NormReasonsRev, RejectReasonsRev} =
        all_of_rule_(Input, Rules, none, none, [], [], State),
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
all_of_rule(_, _, _State) ->
    reject.

-spec all_of_rule_(
        input()
      , [rule()]
      , klsn:optnl(output())
      , klsn:optnl(output())
      , [reason()]
      , [reason()]
      , state()
    ) -> {klsn:optnl(output()), klsn:optnl(output()), [reason()], [reason()]}.
all_of_rule_(_Input, [], MaybeValidOutput, MaybeNormOutput, NormReasonsRev, RejectReasonsRev, _State) ->
    {MaybeValidOutput, MaybeNormOutput, NormReasonsRev, RejectReasonsRev};
all_of_rule_(Input, [Rule|T], MaybeValidOutput0, MaybeNormOutput0, NormReasonsRev0, RejectReasonsRev0, State) ->
    case eval(Input, Rule, State) of
        {valid, Output} ->
            MaybeValidOutput = case MaybeValidOutput0 of
                none ->
                    {value, Output};
                _ ->
                    MaybeValidOutput0
            end,
            all_of_rule_(Input, T, MaybeValidOutput, MaybeNormOutput0, NormReasonsRev0, RejectReasonsRev0, State);
        {normalized, Output, Reason} ->
            MaybeNormOutput = case MaybeNormOutput0 of
                none ->
                    {value, Output};
                _ ->
                    MaybeNormOutput0
            end,
            all_of_rule_(Input, T, MaybeValidOutput0, MaybeNormOutput, [Reason|NormReasonsRev0], RejectReasonsRev0, State);
        {reject, Reason} ->
            all_of_rule_(Input, T, MaybeValidOutput0, MaybeNormOutput0, NormReasonsRev0, [Reason|RejectReasonsRev0], State)
    end.

%% @doc
%% Apply rules left-to-right, feeding each output into the next rule.
%% Rule form: {foldl, [rule()]}.
%%
%% Result (eval/3):
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
%% 1> klsn_rule:eval(ok, {foldl, [atom, {exact, ok}]}, #{}).
%% {valid, ok}
%% 2> klsn_rule:eval("ok", {foldl, [atom, {exact, ok}]}, #{}).
%% {normalized, ok, {invalid, atom, "ok"}}
%% 3> klsn_rule:eval("ok", {foldl, [atom, {exact, nope}]}, #{}).
%% {reject, {invalid_exact, nope, ok}}
%% '''
%% @see validate/2
%% @see normalize/2
%% @see eval/3
-spec foldl_rule(input(), rule_param(), state()) -> result().
foldl_rule(Input, Rules, State) when is_list(Rules) ->
    lists:foldl(fun(Rule, Acc) ->
        case Acc of
            {reject, _} ->
                Acc;
            {valid, Value} ->
                eval(Value, Rule, State);
            {normalized, Value, Reason} ->
                case eval(Value, Rule, State) of
                    {valid, Output} ->
                        {normalized, Output, Reason};
                    {normalized, Output, _Reason} ->
                        {normalized, Output, Reason};
                    {reject, RejectReason} ->
                        {reject, RejectReason}
                end
        end
    end, {valid, Input}, Rules);
foldl_rule(_, _, _State) ->
    reject.

%% @doc
%% Optional rule wrapper; use as {optnl, Rule} in rule() with validate/2,
%% normalize/2, or eval/3. Output is klsn:optnl(Output)
%% ({value, Output} or none).
%%
%% Input handling:
%% - none is valid and stays none.
%% - {value, V} validates V with Rule.
%% - null | nil | undefined | false | [] | error normalize to none.
%% - {ok, V} | {true, V} | [V] | binary() | number() are treated as values,
%%   evaluated with Rule, and wrapped as {value, Output}.
%%
%% Result conditions (eval/3):
%% - valid when Input is none or {value, V} and Rule validates V.
%% - normalized when a wrapper/marker is converted or Rule normalizes V.
%% - reject when Input is unrecognized or Rule rejects V.
%%
%% Reasons (eval/3):
%% - {invalid, optnl, Input} when optnl_rule/3 normalizes without a rule reason
%%   (markers/wrappers that validate) or when Input is unrecognized.
%% - {invalid_optnl_value, Reason} when Rule normalizes or rejects V.
%%
%% validate/2 only accepts valid; normalize/2 returns the output and drops
%% the reason.
%%
%% Examples:
%% ```
%% 1> klsn_rule:eval(none, {optnl, integer}, #{}).
%% {valid, none}
%% 2> klsn_rule:eval(null, {optnl, integer}, #{}).
%% {normalized, none, {invalid, optnl, null}}
%% 3> klsn_rule:eval({ok, 3}, {optnl, integer}, #{}).
%% {normalized, {value, 3}, {invalid, optnl, {ok, 3}}}
%% 4> klsn_rule:eval({value, 3}, {optnl, integer}, #{}).
%% {valid, {value, 3}}
%% 5> klsn_rule:eval({value, foo}, {optnl, integer}, #{}).
%% {reject, {invalid_optnl_value, {invalid, integer, foo}}}
%% 6> klsn_rule:normalize({ok, 3}, {optnl, integer}).
%% {value, 3}
%% '''
%% @see validate/2
%% @see normalize/2
%% @see eval/3
-spec optnl_rule(input(), rule_param(), state()) -> result().
optnl_rule(Input, Rule, State) ->
    case Input of
        {value, Value0} ->
            case eval(Value0, Rule, State) of
                {valid, _} ->
                    valid;
                {normalized, Value, Reason} ->
                    {normalized, {value, Value}, {invalid_optnl_value, Reason}};
                {reject, Reason} ->
                    {reject, {invalid_optnl_value, Reason}}
            end;
        none ->
            valid;
        _ ->
            optnl_rule_(Input, Rule, State)
    end.
optnl_rule_(Input, Rule, State) ->
    InputResult = eval(Input, Rule, State),
    NoneCanary = case eval(none, Rule, State) of
        {valid, _} ->
            %% We can't trust Rule to tell if it's optnl or raw value
            valid;
        {normalized, _, _} ->
            %% We can't trust normalized result, but we can trust valid as raw value.
            normalized;
        {reject, _} ->
            %% We can trust valid and normalized result as raw value
            reject
    end,
    NoneAlt = case eval(Input, {enum, [undefined, null, nil, error, false]}, State) of
        {valid, _} ->
            valid;
        {normalized, _, _} ->
            normalized;
        {reject, _} ->
            reject
    end,
    ValueAltResult = case Input of
        {_, Value0} ->
            {value, eval(Value0, Rule, State)};
        [Value0] ->
            {value, eval(Value0, Rule, State)};
        _ ->
            none
    end,
    case {NoneCanary, NoneAlt, InputResult, ValueAltResult, Input} of
        {valid, _, _, _, _} ->
            reject;
        {_, valid, _, _, _} ->
            {normalized, none};
        {reject, normalized, _, _, _} ->
            {normalized, none};
        {_, _, {reject, _}, {value, {valid, Value}}, _} ->
            {normalized, {value, Value}};
        {_, _, {reject, _}, {value, {normalized, Value, _}}, _} ->
            {normalized, {value, Value}};
        {_, _, {normalized, _, _}, {value, {valid, Value}}, _} ->
            {normalized, {value, Value}};
        {_, _, {reject, _}, _, []} ->
            {normalized, none};
        {_, _, {normalized, [], _}, _, _} ->
            reject;
        {_, _, {normalized, _, _}, _, []} ->
            {normalized, none};
        {reject, _, {valid, Value}, _, _} ->
            {normalized, {value, Value}};
        {reject, _, {normalized, Value, _}, _, _} ->
            {normalized, {value, Value}};
        {normalized, _, {valid, Value}, _, _} ->
            {normalized, {value, Value}};
        _ ->
            reject
    end.

%% @doc
%% Validate a nullable rule used by validate/2, normalize/2, and eval/3.
%% Rule form: {nullable, Rule} where Rule is a rule().
%%
%% Result (eval/3):
%% - valid when Input is null.
%% - normalized to null when Input is none.
%% - valid when Input is Value and Rule validates.
%% - normalized when Input is {value, Value} and Rule validates.
%% - normalized with {invalid_nullable_value, Reason} when Rule normalizes.
%% - reject with {invalid_nullable_value, Reason} when Rule rejects.
%%
%% Reasons (eval/3):
%% - {invalid, nullable, Input} when nullable_rule/3 normalizes without a rule
%%   reason (none or {value, Value} when Rule validates).
%% - {invalid_nullable_value, Reason} when Rule normalizes or rejects Value.
%%
%% Examples:
%% ```
%% 1> klsn_rule:eval(null, {nullable, integer}, #{}).
%% 2> klsn_rule:eval(none, {nullable, integer}, #{}).
%% 3> klsn_rule:eval(<<"12">>, {nullable, integer}, #{}).
%% 4> klsn_rule:eval({value, <<"12">>}, {nullable, integer}, #{}).
%% '''
%% @see validate/2
%% @see normalize/2
%% @see eval/3
-spec nullable_rule(input(), rule_param(), state()) -> result().
nullable_rule(null, _Rule, _State) ->
    valid;
nullable_rule(none, _Rule, _State) ->
    {normalized, null};
nullable_rule({value, Value}, Rule, State) ->
    nullable_eval_value_(Value, Rule, normalized, State);
nullable_rule(Value, Rule, State) ->
    nullable_eval_value_(Value, Rule, valid, State).

%% @doc
%% Enforce strict evaluation when used as {strict, Rule}.
%% Rule form: {strict, Rule} where Rule :: rule().
%%
%% Result (eval/3):
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
%% 1> klsn_rule:eval(42, {strict, integer}, #{}).
%% 2> klsn_rule:eval(<<"42">>, {strict, integer}, #{}).
%% 3> klsn_rule:normalize(42, {strict, integer}).
%% '''
%% @see validate/2
%% @see normalize/2
%% @see eval/3
-spec strict_rule(input(), rule_param(), state()) -> result().
strict_rule(Input, Rule, State) ->
    case eval(Input, Rule, State) of
        {valid, Output} ->
            {valid, Output};
        {normalized, _Output, Reason} ->
            {reject, {strict, Reason}};
        {reject, Reason} ->
            {reject, Reason}
    end.

-spec nullable_eval_value_(input(), rule(), valid | normalized, state()) -> result().
nullable_eval_value_(Value, Rule, Validity, State) ->
    case eval(Value, Rule, State) of
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
%% Result (eval/3):
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
%% 1> klsn_rule:eval([1, 2], {list, integer}, #{}).
%% {valid, [1, 2]}
%% 2> klsn_rule:eval([<<"1">>, <<"2">>], {list, integer}, #{}).
%% {normalized, [1, 2], {invalid_list_element, 1, {invalid, integer, <<"1">>}}}
%% 3> klsn_rule:eval([1, <<"bad">>], {list, integer}, #{}).
%% {reject, {invalid_list_element, 2, {invalid, integer, <<"bad">>}}}
%% 4> klsn_rule:eval(<<"1">>, {list, integer}, #{}).
%% {reject, {invalid, list, <<"1">>}}
%% 5> klsn_rule:normalize([<<"1">>], {list, integer}).
%% [1]
%% 6> klsn_rule:validate([<<"1">>], {list, integer}).
%% ** exception error: {klsn_rule,{invalid_list_element,1,{invalid,integer,<<"1">>}}}
%% '''
%% @see validate/2
%% @see normalize/2
%% @see eval/3
-spec list_rule(input(), rule_param(), state()) -> result().
list_rule(Input, ElementRule, State) when is_list(Input) ->
    List0 = lists:map(fun(Elem) ->
        eval(Elem, ElementRule, State)
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
list_rule(_, _, _State) ->
    reject.

%% @doc
%% Validate tuple inputs against element rules.
%%
%% Rule form: {tuple, Rules} where Rules is a list or tuple of rule().
%%
%% Result (eval/3):
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
%% 1> klsn_rule:eval({1, ok}, {tuple, [integer, atom]}, #{}).
%% {valid, {1, ok}}
%% 2> klsn_rule:eval({"1", ok}, {tuple, [integer, atom]}, #{}).
%% {normalized, {1, ok}, {invalid_tuple_element, 1, {invalid, integer, "1"}}}
%% 3> klsn_rule:eval({1, ok}, {tuple, {integer, atom}}, #{}).
%% {valid, {1, ok}}
%% 4> klsn_rule:eval({1}, {tuple, [integer, atom]}, #{}).
%% {reject, {invalid_tuple_size, 2, {1}}}
%% 5> klsn_rule:eval([1, ok], {tuple, [integer, atom]}, #{}).
%% {reject, {invalid, tuple, [1, ok]}}
%% 6> klsn_rule:normalize({ok, ok}, {tuple, [integer, atom]}).
%% exception error: {klsn_rule,{invalid_tuple_element,1,{invalid,integer,ok}}}
%% '''
%% @see validate/2
%% @see normalize/2
%% @see eval/3
-spec tuple_rule(input(), rule_param(), state()) -> result().
tuple_rule(Input, Rules, State) when is_tuple(Input), is_tuple(Rules) ->
    tuple_rule(Input, tuple_to_list(Rules), State);
tuple_rule(Input, Rules, _State) when is_tuple(Input), is_list(Rules), length(Rules) =/= tuple_size(Input) ->
    {reject, {invalid_tuple_size, length(Rules), Input}};
tuple_rule(Input, Rules, State) when is_tuple(Input), is_list(Rules) ->
    InputList = tuple_to_list(Input),
    List0 = lists:map(fun({Rule, Elem}) ->
        eval(Elem, Rule, State)
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
tuple_rule(_, _, _State) ->
    reject.

%% @doc
%% Validate map input for validate/2, normalize/2, and eval/3.
%% Rule form: {map, {KeyRule, ValueRule}} where KeyRule and ValueRule are rule().
%%
%% Result (eval/3):
%% - valid when Input is a map and all keys/values validate without normalization.
%% - normalized when Input is a map, no key/value rejects, and at least one
%%   key or value normalizes; output is a map with normalized keys/values.
%% - reject when Input is not a map, when a key or value rejects, when
%%   normalized keys collide, or when the rule parameter is not {KeyRule, ValueRule}.
%%
%% Reason (eval/3):
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
%% 1> klsn_rule:eval(#{1 => 2}, {map, {integer, integer}}, #{}).
%% {valid, #{1 => 2}}
%% 2> klsn_rule:eval(#{1 => "2"}, {map, {integer, integer}}, #{}).
%% {normalized, #{1 => 2}, {invalid_map_value, 1, {invalid, integer, "2"}}}
%% 3> klsn_rule:eval(#{"nope" => 1}, {map, {integer, integer}}, #{}).
%% {reject, {invalid_map_key, {invalid, integer, "nope"}}}
%% 4> klsn_rule:eval(#{"1" => a, 1 => b}, {map, {integer, term}}, #{}).
%% {reject, {map_key_conflict, 1}}
%% 5> klsn_rule:eval([1, 2], {map, {integer, integer}}, #{}).
%% {reject, {invalid, map, [1, 2]}}
%% 6> klsn_rule:normalize(#{"1" => "2"}, {map, {integer, integer}}).
%% #{1 => 2}
%% '''
%% @see validate/2
%% @see normalize/2
%% @see eval/3
-spec map_rule(input(), rule_param(), state()) -> result().
map_rule(Input, {KeyRule, ValueRule}, State) when is_map(Input) ->
    List0 = lists:map(fun({Key, Value}) ->
        {Key, eval(Key, KeyRule, State), eval(Value, ValueRule, State)}
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
map_rule(_, _, _State) ->
    reject.

%% @doc
%% Validate struct maps against a field spec for use with validate/2, normalize/2,
%% and eval/3.
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
%% When evaluated via {@link eval/3}:
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
%% 2> klsn_rule:eval(#{name => <<"Ada">>, age => 32}, Rule, #{}).
%% {valid, #{name => <<"Ada">>, age => 32}}
%% 3> klsn_rule:eval(#{<<"name">> => <<"Ada">>, age => <<"42">>}, Rule, #{}).
%% {normalized, #{name => <<"Ada">>, age => 42},
%%  {invalid_struct_field, <<"name">>}}
%% 4> klsn_rule:normalize(#{<<"name">> => <<"Ada">>, extra => 1}, Rule).
%% #{name => <<"Ada">>}
%% 5> klsn_rule:eval(#{age => 32}, Rule, #{}).
%% {reject, {missing_required_field, name}}
%% 6> klsn_rule:eval(#{name => <<"Ada">>, <<"name">> => <<"Ada">>}, Rule, #{}).
%% {reject, {struct_field_conflict, name}}
%% '''
%% @see validate/2
%% @see normalize/2
%% @see eval/3
-spec struct_rule(input(), rule_param(), state()) -> result().
struct_rule(Input, StructSpec, State) when is_map(Input), is_map(StructSpec) ->
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
                            case struct_eval_fields_(SpecList, Matches0, #{}, none, State) of
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
struct_rule(_, _, _State) ->
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
      , state()
    ) -> {ok, maps:map(atom(), term()), klsn:optnl(reason())} | {reject, reason()}.
struct_eval_fields_([], _Matches, Output, MaybeNormReason, _State) ->
    {ok, Output, MaybeNormReason};
struct_eval_fields_([{Field, {_ReqOpt, Rule}}|T], Matches, Output0, MaybeNormReason0, State) ->
    case maps:get(Field, Matches, []) of
        [] ->
            struct_eval_fields_(T, Matches, Output0, MaybeNormReason0, State);
        [{_Key, Value}] ->
            case eval(Value, Rule, State) of
                {valid, ValueOut} ->
                    struct_eval_fields_(T, Matches, maps:put(Field, ValueOut, Output0), MaybeNormReason0, State);
                {normalized, ValueOut, Reason} ->
                    MaybeNormReason1 = case MaybeNormReason0 of
                        none ->
                            {value, {invalid_struct_value, Field, Reason}};
                        _ ->
                            MaybeNormReason0
                    end,
                    struct_eval_fields_(T, Matches, maps:put(Field, ValueOut, Output0), MaybeNormReason1, State);
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
%% tuples; they are dispatched to `name_rule/3' functions. Unknown
%% rules return `{reject, {unknown_rule, Rule}}'.
%%
%% Custom rules use `{custom, Name, Fun, Param}' where
%% `Fun(Input, Param, State) -> result()'. Return handling:
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
%% 1> klsn_rule:eval(<<"10">>, integer, #{}).
%% {normalized,10,{invalid,integer,<<"10">>}}
%% 2> Unwrap = fun({ok, V}, _Param, _State) ->
%%        {normalized, V, {custom, unwrapped}};
%%    (_, _Param, _State) ->
%%        {reject, {custom, unexpected}}
%% end.
%% 3> Rule = {custom, unwrap_ok, Unwrap, []}.
%% 4> klsn_rule:eval({ok, 5}, Rule, #{}).
%% {normalized,5,{custom,unwrapped}}
%% 5> klsn_rule:eval(error, Rule, #{}).
%% {reject,{custom,unexpected}}
%% '''
%% @see validate/2
%% @see normalize/2
-spec eval(input(), rule(), state()) -> strict_result().
eval(Input, {custom, Name, Custom, Param}=Arg2, State) ->
    case Custom(Input, Param, State) of
        valid ->
            {valid, Input};
        {valid, Input} ->
            {valid, Input};
        {valid, Modified} ->
            error({invalid_custom_rule, klsn_binstr:from_any(io_lib:format(
                "custom rule ~p returned {valid, Output}=~p for input ~p, but Output /= Input. "
                "{valid, output()} must return the unmodified input (or return valid)."
              , [Name, Modified, Input]
            ))}, [Arg2, Input]);
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
            ))}, [Arg2, Input])
    end;
eval(Input, Rule, State) ->
    case rule_to_custom_(Rule) of
        {value, CustomRule} ->
            eval(Input, CustomRule, State);
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
        ({FunName, 3})->
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
