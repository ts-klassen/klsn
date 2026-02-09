%% @doc
%% Parse transform for -klsn_input_rule and -klsn_output_rule attributes.
%% Include "klsn/include/klsn_rule_annotation.hrl" to enable.
-module(klsn_rule_annotation).

-export([parse_transform/2]).

-record(state, {
        input = none
      , output = none
    }).

%% @doc Apply the rule annotations to the following function form.
-spec parse_transform([term()], list()) -> [term()].
parse_transform(Forms, _Options) ->
    Module = module_name_(Forms),
    {FormsRev, _State} = lists:foldl(fun(Form, {Acc, State}) ->
        transform_form_(Form, Module, Acc, State)
    end, {[], #state{}}, Forms),
    lists:reverse(FormsRev).

transform_form_({attribute, Line, klsn_input_rule, Rules0}, _Module, Acc, State) ->
    ensure_input_rules_(Rules0, Line),
    {Acc, State#state{input = {Line, Rules0}}};
transform_form_({attribute, Line, klsn_output_rule, Rule}, _Module, Acc, State) ->
    {Acc, State#state{output = {Line, Rule}}};
transform_form_({function, _, _, _, _}=Form, Module, Acc, State) ->
    case State of
        #state{input = none, output = none} ->
            {[Form|Acc], State};
        #state{input = InputRule, output = OutputRule} ->
            NewForms = rewrite_function_(Module, Form, InputRule, OutputRule),
            {lists:reverse(NewForms) ++ Acc, #state{}}
    end;
transform_form_(Form, _Module, Acc, State) ->
    {[Form|Acc], State}.

module_name_(Forms) ->
    case lists:search(fun
        ({attribute, _, module, _}) ->
            true;
        (_) ->
            false
    end, Forms) of
        {value, {attribute, _, module, Module}} ->
            Module;
        false ->
            erlang:error({klsn_rule_annotation, missing_module_attribute})
    end.

ensure_input_rules_(Rules, _Line) when is_list(Rules) ->
    ok;
ensure_input_rules_(Rules, Line) ->
    erlang:error({klsn_input_rule, Line, invalid_input_rule, Rules}).

rewrite_function_(Module, {function, Line, Name, Arity, Clauses}, InputRule, OutputRule) ->
    InputRules = case InputRule of
        none ->
            none;
        {_, Rules} ->
            Rules
    end,
    OutputRuleTerm = case OutputRule of
        none ->
            none;
        {_, Rule} ->
            Rule
    end,
    InputRules1 = ensure_input_arity_(InputRules, Arity, Line, Name),
    OrigName = original_name_(Name, Arity),
    OrigFun = {function, Line, OrigName, Arity, Clauses},
    WrapperFun = build_wrapper_(Line, Module, Name, Arity, OrigName, InputRules1, OutputRuleTerm),
    [WrapperFun, OrigFun].

ensure_input_arity_(none, _Arity, _Line, _Name) ->
    none;
ensure_input_arity_(Rules, Arity, Line, Name) when is_list(Rules) ->
    case length(Rules) =:= Arity of
        true ->
            Rules;
        false ->
            erlang:error({klsn_input_rule, Line, {arity_mismatch, Name, Arity, length(Rules)}})
    end;
ensure_input_arity_(Rules, _Arity, Line, Name) ->
    erlang:error({klsn_input_rule, Line, {invalid_rule_list, Name, Rules}}).

original_name_(Name, _Arity) ->
    list_to_atom("__klsn_rule_annotation__orig__" ++ atom_to_list(Name)).

build_wrapper_(Line, Module, Name, Arity, OrigName, InputRules, OutputRule) ->
    ArgVars = arg_vars_(Line, Arity),
    ArgsListExpr = list_ast_(Line, ArgVars),
    {InputExprs, CallArgs} = build_input_validations_(
        Line, Module, Name, ArgsListExpr, InputRules, ArgVars
    ),
    ResultVar = var_(Line, 'Result'),
    OrigCall = call_local_(Line, OrigName, CallArgs),
    ResultMatch = {match, Line, ResultVar, OrigCall},
    Body = case OutputRule of
        none ->
            InputExprs ++ [ResultMatch];
        _ ->
            OutputExpr = build_output_validation_(
                Line, Module, Name, ArgsListExpr, ResultVar, OutputRule
            ),
            InputExprs ++ [ResultMatch, OutputExpr]
    end,
    Clause = {clause, Line, ArgVars, [], Body},
    {function, Line, Name, Arity, [Clause]}.

arg_vars_(Line, Arity) ->
    [var_(Line, list_to_atom("Arg" ++ integer_to_list(I))) || I <- lists:seq(1, Arity)].

norm_vars_(Line, Arity) ->
    [var_(Line, list_to_atom("Arg" ++ integer_to_list(I) ++ "N")) || I <- lists:seq(1, Arity)].

build_input_validations_(_Line, _Module, _Name, _ArgsListExpr, none, ArgVars) ->
    {[], ArgVars};
build_input_validations_(Line, Module, Name, ArgsListExpr, Rules, ArgVars) ->
    NormVars = norm_vars_(Line, length(ArgVars)),
    ExprsRev = build_input_exprs_(
        Line, Module, Name, ArgsListExpr, Rules, ArgVars, NormVars, 1, []
    ),
    {lists:reverse(ExprsRev), NormVars}.

build_input_exprs_(_Line, _Module, _Name, _ArgsListExpr, [], [], [], _Index, Acc) ->
    Acc;
build_input_exprs_(Line, Module, Name, ArgsListExpr,
                   [Rule|Rules], [ArgVar|ArgVars], [NormVar|NormVars], Index, Acc) ->
    Expr = input_match_expr_(
        Line, Module, Name, ArgsListExpr, Index, Rule, ArgVar, NormVar
    ),
    build_input_exprs_(
        Line, Module, Name, ArgsListExpr, Rules, ArgVars, NormVars, Index + 1, [Expr|Acc]
    ).

input_match_expr_(Line, Module, Name, ArgsListExpr, Index, Rule, ArgVar, NormVar) ->
    EvalCall = call_remote_(
        Line,
        klsn_rule,
        eval,
        [ArgVar, erl_parse:abstract(Rule, Line), erl_parse:abstract(#{}, Line)]
    ),
    NormValueVar = var_(Line, list_to_atom("NormValue" ++ integer_to_list(Index))),
    ReasonVar = var_(Line, list_to_atom("Reason" ++ integer_to_list(Index))),
    ErrorTuple = tuple_ast_(Line, [
        atom_(Line, klsn_input_rule),
        tuple_ast_(Line, [atom_(Line, Module), atom_(Line, Name), ArgsListExpr]),
        integer_(Line, Index),
        ReasonVar
    ]),
    ErrorCall = call_remote_(Line, erlang, error, [ErrorTuple]),
    CaseExpr = {'case', Line, EvalCall, [
        {clause, Line, [tuple_ast_(Line, [atom_(Line, valid), var_(Line, '_')])], [], [ArgVar]},
        {clause, Line, [tuple_ast_(Line, [atom_(Line, normalized), NormValueVar, var_(Line, '_')])], [], [NormValueVar]},
        {clause, Line, [tuple_ast_(Line, [atom_(Line, reject), ReasonVar])], [], [ErrorCall]}
    ]},
    {match, Line, NormVar, CaseExpr}.

build_output_validation_(Line, Module, Name, ArgsListExpr, ResultVar, Rule) ->
    EvalCall = call_remote_(
        Line,
        klsn_rule,
        eval,
        [ResultVar, erl_parse:abstract(Rule, Line), erl_parse:abstract(#{}, Line)]
    ),
    NormValueVar = var_(Line, 'NormValueOut'),
    ReasonVar = var_(Line, 'ReasonOut'),
    ErrorTuple = tuple_ast_(Line, [
        atom_(Line, klsn_output_rule),
        tuple_ast_(Line, [atom_(Line, Module), atom_(Line, Name), ArgsListExpr]),
        ResultVar,
        ReasonVar
    ]),
    ErrorCall = call_remote_(Line, erlang, error, [ErrorTuple]),
    {'case', Line, EvalCall, [
        {clause, Line, [tuple_ast_(Line, [atom_(Line, valid), var_(Line, '_')])], [], [ResultVar]},
        {clause, Line, [tuple_ast_(Line, [atom_(Line, normalized), NormValueVar, var_(Line, '_')])], [], [NormValueVar]},
        {clause, Line, [tuple_ast_(Line, [atom_(Line, reject), ReasonVar])], [], [ErrorCall]}
    ]}.

atom_(Line, Atom) ->
    {atom, Line, Atom}.

var_(Line, Name) ->
    {var, Line, Name}.

integer_(Line, Value) ->
    {integer, Line, Value}.

tuple_ast_(Line, Elements) ->
    {tuple, Line, Elements}.

list_ast_(Line, []) ->
    {nil, Line};
list_ast_(Line, [H|T]) ->
    {cons, Line, H, list_ast_(Line, T)}.

call_remote_(Line, Module, Function, Args) ->
    {call, Line, {remote, Line, atom_(Line, Module), atom_(Line, Function)}, Args}.

call_local_(Line, Function, Args) ->
    {call, Line, atom_(Line, Function), Args}.
