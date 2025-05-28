-module(klsn_code).

-export_type([
        sample_type/0
      , sample_type/1
      , sample_type/2
    ]).

-export([
        sample_function/0
      , sample_function/1
      , type/1
      , spec/1
      , function/1
    ]).

-type sample_type() :: sample_type_payload.
-type sample_type(A) :: A.
-type sample_type(A, B) :: {A,B}.

-spec sample_function() -> sample_function_return.
sample_function() ->
    sample_function_return.

-spec sample_function(sample_function_arg1) -> sample_function_return1;
                     (sample_function_arg2) -> sample_function_return2.
sample_function(sample_function_arg1) ->
    sample_function_return1;
sample_function(sample_function_arg2) ->
    sample_function_return2.

%% Retrieve the type definition for a given type name and arity.
-spec type({module(), atom(), non_neg_integer()}) -> {atom(), term(), [term()]}.
type({Module, Name, Arity}) when is_atom(Module), is_atom(Name), is_integer(Arity) ->
    try abstract_code(Module) of
        Forms when is_list(Forms) ->
            type_from_forms(Forms, Module, Name, Arity)
    catch
        _:_ ->
            erlang:error(no_abstract_code, [Module, Name, Arity])
    end.

type_from_forms(Forms, Module, Name, Arity) ->
    Types = [ {TName, TExpr, Vars}
              || {attribute, _, type, {TName, TExpr, Vars}} <- Forms,
                 TName == Name,
                 length(Vars) =:= Arity ],
    case Types of
        [Result] -> Result;
        [] -> erlang:error(undefined_type, [Module, Name, Arity]);
        [Result|_] -> Result
    end.

%% Retrieve the spec (type signatures) for a given function name and arity.
-spec spec({module(), atom(), non_neg_integer()}) -> [term()].
spec({Module, Name, Arity}) when is_atom(Module), is_atom(Name), is_integer(Arity) ->
    try abstract_code(Module) of
        Forms when is_list(Forms) ->
            spec_from_forms(Forms, Name, Arity)
    catch
        _:_ ->
            erlang:error(no_abstract_code, [Module, Name, Arity])
    end.

spec_from_forms(Forms, Name, Arity) ->
    SpecsList = [ Specs
                  || {attribute, _, spec, {{FName, FArity}, Specs}} <- Forms,
                     FName == Name,
                     FArity =:= Arity ],
    case SpecsList of
        [Specs] -> Specs;
        [] -> [];
        [Specs|_] -> Specs
    end.

%% Retrieve the clauses (implementations) for a given function name and arity.
-spec function({module(), atom(), non_neg_integer()}) -> [term()].
function({Module, Name, Arity}) when is_atom(Module), is_atom(Name), is_integer(Arity) ->
    try abstract_code(Module) of
        Forms when is_list(Forms) ->
            function_from_forms(Forms, Name, Arity)
    catch
        _:_ ->
            erlang:error(no_abstract_code, [Module, Name, Arity])
    end.

function_from_forms(Forms, Name, Arity) ->
    ClausesList = [ Clauses
                    || {function, _, FName, FArity, Clauses} <- Forms,
                       FName == Name,
                       FArity =:= Arity ],
    case ClausesList of
        [Clauses] -> Clauses;
        [] -> [];
        [Clauses|_] -> Clauses
    end.

%% Internal: load and return the raw abstract code forms for a module.
-spec abstract_code(module()) -> [term()].
abstract_code(Module) when is_atom(Module) ->
    {_, Bin, _} = code:get_object_code(Module),
    {ok, {_, [{abstract_code, Data}]}} = beam_lib:chunks(Bin, [abstract_code]),
    case Data of
        {raw_abstract_v1, Forms} -> Forms;
        Forms when is_list(Forms) -> Forms
    end.

