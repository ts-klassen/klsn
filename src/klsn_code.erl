-module(klsn_code).

-export([
        type/1
      , spec/1
      , function/1
    ]).

%% Retrieve the type definition for a given type name and arity.
-spec type({module(), atom(), non_neg_integer()}) -> {atom(), term(), [term()]}.
type({Module, Name, Arity}) when is_atom(Module), is_atom(Name), is_integer(Arity) ->
    Forms = abstract_code(Module),
    Types = [ {TName, TExpr, Vars}
              || {attribute, _, type, {TName, TExpr, Vars}} <- Forms,
                 TName == Name,
                 length(Vars) =:= Arity ],
    case Types of
        [] -> erlang:error(undefined_type, [{Module, Name, Arity}]);
        [Result|_] -> Result
    end.

%% Retrieve the spec (type signatures) for a given function name and arity.
-spec spec({module(), atom(), non_neg_integer()}) -> [term()].
spec({Module, Name, Arity}) when is_atom(Module), is_atom(Name), is_integer(Arity) ->
    Forms = abstract_code(Module),
    SpecsList = [ Specs
                  || {attribute, _, spec, {{FName, FArity}, Specs}} <- Forms,
                     FName == Name,
                     FArity =:= Arity ],
    case SpecsList of
        [] -> erlang:error(undefined_spec, [{Module, Name, Arity}]);
        [Specs|_] -> Specs
    end.

%% Retrieve the clauses (implementations) for a given function name and arity.
-spec function({module(), atom(), non_neg_integer()}) -> [term()].
function({Module, Name, Arity}) when is_atom(Module), is_atom(Name), is_integer(Arity) ->
    Forms = abstract_code(Module),
    ClausesList = [ Clauses
                    || {function, _, FName, FArity, Clauses} <- Forms,
                       FName == Name,
                       FArity =:= Arity ],
    case ClausesList of
        [] -> erlang:error(undefined_function, [{Module, Name, Arity}]);
        [Clauses|_] -> Clauses
    end.

%% Internal: load and return the raw abstract code forms for a module.
-spec abstract_code(module()) -> [term()].
abstract_code(Module) when is_atom(Module) ->
    case code:get_object_code(Module) of
        {_, Bin, _} ->
            case beam_lib:chunks(Bin, [abstract_code]) of
                {ok, {_, [{abstract_code, Data}]}} ->
                    case Data of
                        {raw_abstract_v1, Forms} when is_list(Forms) ->
                            Forms;
                        Forms when is_list(Forms) ->
                            Forms;
                        _ ->
                            erlang:error(no_abstract_code, [Module])
                    end;
                _ ->
                    erlang:error(no_abstract_code, [Module])
            end;
        _ ->
            erlang:error(no_abstract_code, [Module])
    end.

