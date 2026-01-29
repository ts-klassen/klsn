-module(klsn_obj).

-export([
        get/2
      , lookup/2
      , find/2
      , crud/3
      , validate/2
      , normalize/2
    ]).

-export_type([
        value/0
      , key/0
      , nth/0
      , obj/0
      , cmd/0
      , path/0
      , find_fun/0
      , crud_fun/0
      , rule/0
      , reject_reason/0
    ]).

%% ------------------------------------------------------------------
%% Exported types
%% ------------------------------------------------------------------

%% Any Erlang term that can live inside a nested structure tracked by this
%% module.
-type value() :: term().

%% Key used in a map.
-type key() :: term().

%% 1-based index into a list or tuple.
-type nth() :: pos_integer().

%% JSON-like recursive data structure: atoms, binaries, maps, lists or
%% tuples of other obj() values.
-type obj() :: value()
             | lists:list(obj())
             | tuple() % {}, {obj()}, {obj(), obj()}, ...
             | maps:map(key(), obj())
             .

%% A single navigation step used in a path().
-type cmd() :: {raw, nth() | key()}
             | {map, key()} | {m, key()}
             | {list, nth()} | {l, nth()}
             | {tuple, nth()} | {t, nth()}
             | nth() | key()
             .

%% List of navigation commands that drills down into an obj().
-type path() :: [cmd()].

%% A shortened path returned by find/2 where each step is unambiguous.
-type short_path() :: [{m, key()} | {l|t, nth()}].

%% Predicate used by find/2. It can accept either the current value only
%% or the value together with its short path.
-type find_fun() :: fun((value())->boolean())
                  | fun((value(), short_path())->boolean())
                  .

%% Callback used by crud/3 to create, update or delete a value at the
%% given path.
-type crud_fun() :: fun((klsn:'maybe'(value()))->klsn:'maybe'(value())).


-type rule() :: any
              | integer
              | to_integer
              | float
              | to_float
              | number
              | to_number
              | binstr
              | to_binstr
              | existing_atom
              | to_existing_atom
              | {atom, [atom()]}
              | {to_atom, [atom()]}
              | #{ atom() := {r|required | o|optional, rule()} }
              | {map, rule(), rule()}
              | {list, rule()}
              | {tuple, [rule()]} % Also allow {tuple, {rule(), ...}}
              | {optnl, rule()}
              | timeout
              | boolean
              .

-type reject_reason() :: {invalid, rule(), term()}
                       | {invalid_field, atom(), reject_reason()}
                       | {missing_required_field, atom(), term()}
                       | {invalid_list_element, pos_integer(), reject_reason()}
                       | {invalid_tuple_element, pos_integer(), reject_reason()}
                       | {invalid_optnl_value, reject_reason()}
                       | {duplicated_map_keys, [term()]}
                       | {invalid_map_key, reject_reason()}
                       | {invalid_map_value, term(), reject_reason()}
                       .


%% @doc
%% Safe navigation: returns {value, V} when the element exists, otherwise
%% none.
-spec lookup(path(), obj()) -> klsn:'maybe'(value()).
lookup(Path, Obj) ->
    try get(Path, Obj) of
        Value ->
            {value, Value}
    catch
        error:not_found ->
            none
    end.

%% @doc
%% Navigate Obj using Path and return the value. Raises
%% error:not_found when any step is invalid.
-spec get(path(), obj()) -> value().
get([], Value) ->
    Value;
get([H|T], Map) when is_map(Map) ->
    Key = case H of
        {raw, Key0} -> Key0;
        {map, Key0} -> Key0;
        {m, Key0} -> Key0;
        {list, _} -> erlang:error(not_found, [[H|T], Map]);
        {l, _} -> erlang:error(not_found, [[H|T], Map]);
        {tuple, _} -> erlang:error(not_found, [[H|T], Map]);
        {t, _} -> erlang:error(not_found, [[H|T], Map]);
        Key0 -> Key0
    end,
    case maps:find(Key, Map) of
        {ok, Value} ->
            get(T, Value);
        error ->
            erlang:error(not_found, [[H|T], Map])
    end;
get([H|T], List) when is_list(List) ->
    Nth = case H of
        {raw, Key0} when is_integer(Key0), (Key0 > 0) -> Key0;
        {map, _} -> erlang:error(not_found, [[H|T], List]);
        {m, _} -> erlang:error(not_found, [[H|T], List]);
        {list, Key0} when is_integer(Key0), (Key0 > 0) -> Key0;
        {l, Key0} when is_integer(Key0), (Key0 > 0) -> Key0;
        {tuple, _} -> erlang:error(not_found, [[H|T], List]);
        {t, _} -> erlang:error(not_found, [[H|T], List]);
        Key0 when is_integer(Key0), (Key0 > 0) -> Key0;
        _ -> erlang:error(not_found, [[H|T], List])
    end,
    try lists:nth(Nth, List) of
        Value ->
            get(T, Value)
    catch
        error:function_clause ->
            erlang:error(not_found, [[H|T], List])
    end;
get([H|T], Tuple) when is_tuple(Tuple) ->
    Nth = case H of
        {raw, Key0} when is_integer(Key0), (Key0 > 0) -> Key0;
        {map, _} -> erlang:error(not_found, [[H|T], Tuple]);
        {m, _} -> erlang:error(not_found, [[H|T], Tuple]);
        {list, _} -> erlang:error(not_found, [[H|T], Tuple]);
        {l, _} -> erlang:error(not_found, [[H|T], Tuple]);
        {tuple, Key0} when is_integer(Key0), (Key0 > 0) -> Key0;
        {t, Key0} when is_integer(Key0), (Key0 > 0) -> Key0;
        Key0 when is_integer(Key0), (Key0 > 0) -> Key0;
        _ -> erlang:error(not_found, [[H|T], Tuple])
    end,
    try element(Nth, Tuple) of
        Value ->
            get(T, Value)
    catch
        error:badarg ->
            erlang:error(not_found, [[H|T], Tuple])
    end;
get(Arg1, Arg2) ->
    erlang:error(not_found, [Arg1, Arg2]).

%% @doc
%% Depth-first search for every occurrence that satisfies *FindFun*.
%% Returns a list of *short paths* to the matches.
-spec find(find_fun(), obj()) -> [short_path()].
find(FindFun, Obj) when is_function(FindFun, 1) ->
    find(fun(Value, _Path) -> FindFun(Value) end, Obj);
find(FindFun, Obj) when is_function(FindFun, 2) ->
    find_dfs(FindFun, Obj, []).

% Ignore the performance for now. Just get it to work.
-spec find_dfs(
        fun((value(), short_path())->boolean())
      , obj()
      , path()
    ) -> [short_path()].
find_dfs(FindFun, Obj, Path) ->
    Acc = case FindFun(Obj, Path) of
        true ->
            [Path];
        false ->
            []
    end,
    case Obj of
        Map when is_map(Map) ->
            maps:fold(fun(Key, Elem, A)->
                A ++ find_dfs(FindFun, Elem, Path ++ [{m, Key}])
            end, Acc, Map);
        List when is_list(List) ->
            IList = lists:zip(lists:seq(1, length(List)), List),
            lists:foldl(fun({I, Elem}, A)->
                A ++ find_dfs(FindFun, Elem, Path ++ [{l, I}])
            end, Acc, IList);
        Tuple when is_tuple(Tuple) ->
            List = tuple_to_list(Tuple),
            IList = lists:zip(lists:seq(1, length(List)), List),
            lists:foldl(fun({I, Elem}, A)->
                A ++ find_dfs(FindFun, Elem, Path ++ [{t, I}])
            end, Acc, IList);
        _ ->
            Acc
    end.

%% @doc
%% "Create-Read-Update-Delete" helper. Applies CRUDFun on the existing
%% value (or none when the path is missing) and rebuilds the object with
%% the returned result.
-spec crud(path(), crud_fun(), obj()) -> obj().
crud(Path, CRUDFun, Obj) ->
    {PathLeft, MaybeValue, History} = try crud_history(Path, Obj, []) catch
        throw:{?MODULE, Args} ->
            Args
    end,
    MaybeUpdatedValue = CRUDFun(MaybeValue),
    case MaybeUpdatedValue of
        {value, _} ->
            ok;
        none ->
            ok;
        Other ->
            erlang:error({bad_return, #{type => {?MODULE, crud_fun, 0}, return => Other, msg => <<"Return of klsn_obj:crud_fun() must be `{value, klsn_obj:value()}` or `none`.">>}})
    end,
    crud_build(lists:reverse(PathLeft), MaybeUpdatedValue, History).


-spec crud_history(
        path(), obj(), [{short_path(), obj()}]
    ) -> {path(), klsn:'maybe'(value()), [{short_path(), obj()}]}.
crud_history([], Value, History) ->
    {[], {value, Value}, History};
crud_history([H|T], Map, History) when is_map(Map) ->
    Key = case H of
        {raw, Key0} -> Key0;
        {map, Key0} -> Key0;
        {m, Key0} -> Key0;
        {list, _} -> erlang:throw({?MODULE, {[H|T], none, History}});
        {l, _} -> erlang:throw({?MODULE, {[H|T], none, History}});
        {tuple, _} -> erlang:throw({?MODULE, {[H|T], none, History}});
        {t, _} -> erlang:throw({?MODULE, {[H|T], none, History}});
        Key0 -> Key0
    end,
    case maps:find(Key, Map) of
        {ok, Value} ->
            crud_history(T, Value, [{{m,Key},Map}|History]);
        error ->
            {T, none, [{{m,Key},Map}|History]}
    end;
crud_history([H|T], List, History) when is_list(List) ->
    Nth = case H of
        {raw, Key0} when is_integer(Key0), (Key0 > 0) -> Key0;
        {map, _} -> erlang:throw({?MODULE, {[H|T], none, History}});
        {m, _} -> erlang:throw({?MODULE, {[H|T], none, History}});
        {list, Key0} when is_integer(Key0), (Key0 > 0) -> Key0;
        {l, Key0} when is_integer(Key0), (Key0 > 0) -> Key0;
        {tuple, _} -> erlang:throw({?MODULE, {[H|T], none, History}});
        {t, _} -> erlang:throw({?MODULE, {[H|T], none, History}});
        Key0 when is_integer(Key0), (Key0 > 0) -> Key0;
        _ -> erlang:throw({?MODULE, {[H|T], none, History}})
    end,
    try lists:nth(Nth, List) of
        Value ->
            crud_history(T, Value, [{{l,Nth},List}|History])
    catch
        error:function_clause ->
            {T, none, [{{l,Nth},List}|History]}
    end;
crud_history([H|T], Tuple, History) when is_tuple(Tuple) ->
    Nth = case H of
        {raw, Key0} when is_integer(Key0), (Key0 > 0) -> Key0;
        {map, _} -> erlang:throw({?MODULE, {[H|T], none, History}});
        {m, _} -> erlang:throw({?MODULE, {[H|T], none, History}});
        {list, _} -> erlang:throw({?MODULE, {[H|T], none, History}});
        {l, _} -> erlang:throw({?MODULE, {[H|T], none, History}});
        {tuple, Key0} when is_integer(Key0), (Key0 > 0) -> Key0;
        {t, Key0} when is_integer(Key0), (Key0 > 0) -> Key0;
        Key0 when is_integer(Key0), (Key0 > 0) -> Key0;
        _ -> erlang:throw({?MODULE, {[H|T], none, History}})
    end,
    try element(Nth, Tuple) of
        Value ->
            crud_history(T, Value, [{{t,Nth},Tuple}|History])
    catch
        error:badarg ->
            {T, none, [{{t,Nth},Tuple}|History]}
    end;
crud_history(Path, _Value, History) ->
    {Path, none, History}.


-spec crud_build(
        klsn:'maybe'(obj())
      , [{short_path(), obj()}]
    ) -> obj().
crud_build(none, []) ->
    nil;
crud_build({value, Obj}, []) ->
    Obj;
crud_build(none, [{{m,Key},Map}|Tail]) when is_map(Map) ->
    crud_build({value, maps:remove(Key, Map)}, Tail);
crud_build(none, [{{l,Nth},List}|Tail]) when is_list(List) ->
    crud_build({value, delete_nth(Nth, List)}, Tail);
crud_build(none, [{{t,Nth},Tuple}|Tail]) when is_tuple(Tuple) ->
    crud_build({value, delete_nth(Nth, Tuple)}, Tail);
crud_build({value, Value}, [{{m,Key},Map}|Tail]) when is_map(Map) ->
    crud_build({value, Map#{ Key => Value }}, Tail);
crud_build({value, Value}, [{{l,Nth},List}|Tail]) when is_list(List) ->
    crud_build({value, replace_nth(Nth, Value, List)}, Tail);
crud_build({value, Value}, [{{t,Nth},Tuple}|Tail]) when is_tuple(Tuple) ->
    crud_build({value, replace_nth(Nth, Value, Tuple)}, Tail).

-spec crud_build(
        Reversed::path()
      , klsn:'maybe'(value())
      , [{short_path(), obj()}]
    ) -> obj().
crud_build(_, none, History) ->
    crud_build(none, History);
crud_build([], MaybeValue, History) ->
    crud_build(MaybeValue, History);
crud_build([Cmd|Tail], {value, Value}, History) ->
    ShortCmd = case Cmd of
        {raw, Key0} -> {m,Key0};
        {map, Key0} -> {m,Key0};
        {m, Key0} -> {m,Key0};
        {list, Key0} when is_integer(Key0), (Key0 > 0) -> {l,Key0};
        {l, Key0} when is_integer(Key0), (Key0 > 0) -> {l,Key0};
        {tuple, Key0} when is_integer(Key0), (Key0 > 0) -> {t,Key0};
        {t, Key0} when is_integer(Key0), (Key0 > 0) -> {t,Key0};
        Key0 -> {m,Key0}
    end,
    UpdatedValue = case ShortCmd of
        {m,Key} ->
            #{ Key => Value };
        {l,Nth} ->
            replace_nth(Nth, Value, []);
        {t,Nth} ->
            replace_nth(Nth, Value, {})
    end,
    crud_build(Tail, {value, UpdatedValue}, History).


replace_nth(Index, NewElement, Tuple) when is_tuple(Tuple) ->
    list_to_tuple(replace_nth(Index, NewElement, tuple_to_list(Tuple)));
replace_nth(Index, NewElement, List) when is_integer(Index), Index >= 1, is_list(List) ->
    ListLen = length(List),
    case Index =< ListLen of
        true  ->
            Prefix = lists:sublist(List, Index - 1),
            Tail   = lists:nthtail(Index, List),
            Prefix ++ [NewElement] ++ Tail;
        false ->
            PadLen = Index - ListLen - 1,
            NilPad = lists:duplicate(PadLen, nil),
            List ++ NilPad ++ [NewElement]
    end.


delete_nth(Index, Tuple) when is_tuple(Tuple) ->
    list_to_tuple(delete_nth(Index, tuple_to_list(Tuple)));
delete_nth(Index, List) when is_integer(Index), Index >= 1, is_list(List) ->
    ListLen = length(List),
    case Index =< ListLen of
        true  ->
            Prefix = lists:sublist(List, Index - 1),
            Tail   = lists:nthtail(Index, List),
            Prefix ++ Tail;
        false ->
            List
    end.

-spec validate(rule(), term()) -> boolean().
validate(Rule, Input) ->
    try
        parse_rule(Rule, Input)
    of
        _ ->
            true
    catch
        throw:{?MODULE, _Reason} ->
            false
    end.

-spec normalize(rule(), term()) -> term().
normalize(Rule, Input) ->
    try parse_rule(Rule, Input) catch
        throw:{?MODULE, Reason} ->
            erlang:error({?MODULE, reject_reason, Reason})
    end.

parse_rule(integer, Valid) when is_integer(Valid) ->
    Valid;
parse_rule(to_integer, Valid) when is_integer(Valid) ->
    Valid;
parse_rule(to_integer, Binary) when is_binary(Binary) ->
    try binary_to_integer(Binary) catch
        _:_ ->
            throw({?MODULE, {invalid, to_integer, Binary}})
    end;
parse_rule(to_integer, List) when is_list(List) ->
    try list_to_integer(List) catch
        _:_ ->
            throw({?MODULE, {invalid, to_integer, List}})
    end;
parse_rule(float, Valid) when is_float(Valid) ->
    Valid;
parse_rule(to_float, Valid) when is_float(Valid) ->
    Valid;
parse_rule(to_float, Binary) when is_binary(Binary) ->
    try binary_to_float(Binary) catch
        _:_ ->
            throw({?MODULE, {invalid, to_float, Binary}})
    end;
parse_rule(to_float, List) when is_list(List) ->
    try list_to_float(List) catch
        _:_ ->
            throw({?MODULE, {invalid, to_float, List}})
    end;
parse_rule(number, Valid) when is_number(Valid) ->
    Valid;
parse_rule(to_number, Valid) when is_number(Valid) ->
    Valid;
parse_rule(to_number, Binary) when is_binary(Binary) ->
    try binary_to_integer(Binary) catch
        _:_ ->
            try binary_to_float(Binary) catch
                _:_ ->
                    throw({?MODULE, {invalid, to_number, Binary}})
            end
    end;
parse_rule(to_number, List) when is_list(List) ->
    try list_to_integer(List) catch
        _:_ ->
            try list_to_float(List) catch
                _:_ ->
                    throw({?MODULE, {invalid, to_number, List}})
            end
    end;
parse_rule(timeout, Valid) when is_integer(Valid), Valid >= 0 ->
    Valid;
parse_rule(timeout, infinity) ->
    infinity;
parse_rule(boolean, true) ->
    true;
parse_rule(boolean, false) ->
    false;
parse_rule(binstr, Valid) when is_binary(Valid) ->
    Valid;
parse_rule(to_binstr, Valid) when is_binary(Valid) ->
    Valid;
parse_rule(to_binstr, Atom) when is_atom(Atom) ->
    atom_to_binary(Atom, utf8);
parse_rule(to_binstr, Int) when is_integer(Int) ->
    integer_to_binary(Int);
parse_rule(to_binstr, Float) when is_float(Float) ->
    float_to_binary(Float);
parse_rule(to_binstr, List) when is_list(List) ->
    try list_to_binary(List) catch
        _:_ ->
            throw({?MODULE, {invalid, to_binstr, List}})
    end;
parse_rule(existing_atom, Valid) when is_atom(Valid) ->
    Valid;
parse_rule(to_existing_atom, Valid) when is_atom(Valid) ->
    Valid;
parse_rule(to_existing_atom, Binary) when is_binary(Binary) ->
    try binary_to_existing_atom(Binary, utf8) catch
        _:_ ->
            throw({?MODULE, {invalid, to_existing_atom, Binary}})
    end;
parse_rule(to_existing_atom, List) when is_list(List) ->
    try binary_to_existing_atom(list_to_binary(List), utf8) catch
        _:_ ->
            throw({?MODULE, {invalid, to_existing_atom, List}})
    end;
parse_rule({atom, AllowedAtoms}, Atom) when is_atom(Atom) ->
    case lists:member(Atom, AllowedAtoms) of
        true ->
            Atom;
        false ->
            throw({?MODULE, {invalid, {atom, AllowedAtoms}, Atom}})
    end;
parse_rule({to_atom, AllowedAtoms}, Atom) when is_atom(Atom) ->
    case lists:member(Atom, AllowedAtoms) of
        true ->
            Atom;
        false ->
            throw({?MODULE, {invalid, {to_atom, AllowedAtoms}, Atom}})
    end;
parse_rule({to_atom, AllowedAtoms}, Binary) when is_binary(Binary) ->
    try binary_to_existing_atom(Binary, utf8) of
        Atom ->
            case lists:member(Atom, AllowedAtoms) of
                true ->
                    Atom;
                false ->
                    throw({?MODULE, {invalid, {to_atom, AllowedAtoms}, Binary}})
            end
    catch
        _:_ ->
            throw({?MODULE, {invalid, {to_atom, AllowedAtoms}, Binary}})
    end;
parse_rule({to_atom, AllowedAtoms}, List) when is_list(List) ->
    try binary_to_existing_atom(list_to_binary(List), utf8) of
        Atom ->
            case lists:member(Atom, AllowedAtoms) of
                true ->
                    Atom;
                false ->
                    throw({?MODULE, {invalid, {to_atom, AllowedAtoms}, List}})
            end
    catch
        _:_ ->
            throw({?MODULE, {invalid, {to_atom, AllowedAtoms}, List}})
    end;
parse_rule(Rule, Map) when is_map(Rule), is_map(Map) ->
    maps:filtermap(fun
        (Key, {R, ValRule}) when is_atom(Key), (R=:=r orelse R=:=required) ->
            case lookup_map(Key, Map) of
                {value, Value} ->
                   try {true, parse_rule(ValRule, Value)} catch
                       throw:{?MODULE, Reason} ->
                           throw({?MODULE, {invalid_field, Key, Reason}})
                   end;
                none ->
                    throw({?MODULE, {missing_required_field, Key, Map}})
            end;
        (Key, {O, ValRule}) when is_atom(Key), (O=:=o orelse O=:=optional) ->
            case lookup_map(Key, Map) of
                {value, Value} ->
                   try {true, parse_rule(ValRule, Value)} catch
                       throw:{?MODULE, Reason} ->
                           throw({?MODULE, {invalid_field, Key, Reason}})
                   end;
                none ->
                    false
            end;
        (_, _) ->
            throw({?MODULE, {invalid, Rule, Map}})
    end, Rule);
parse_rule({map, KeyRule, ValRule}, Map) when is_map(Map) ->
    {ResultMap, _SeenKeys, DuplicatedKeys} =
        lists:foldl(fun({Key0, Value0}, {AccMap, SeenKeys, DupKeys}) ->
            Key = try parse_rule(KeyRule, Key0) catch
                throw:{?MODULE, Reason0} ->
                    throw({?MODULE, {invalid_map_key, Reason0}})
            end,
            case lists:member(Key, SeenKeys) orelse maps:is_key(Key, AccMap) of
                true ->
                    {AccMap, SeenKeys, [Key | DupKeys]};
                false ->
                    Value = try parse_rule(ValRule, Value0) catch
                        throw:{?MODULE, Reason} ->
                            throw({?MODULE, {invalid_map_value, Key, Reason}})
                    end,
                    {maps:put(Key, Value, AccMap), [Key | SeenKeys], DupKeys}
            end
        end, {#{}, [], []}, maps:to_list(Map)),
    case DuplicatedKeys of
        [] ->
            ResultMap;
        _ ->
            throw({?MODULE, {invalid_map_key, {duplicated_map_keys, DuplicatedKeys}}})
    end;
parse_rule({list, Rule}, List) when is_list(List) ->
    lists:map(fun({I, Element}) ->
        try parse_rule(Rule, Element) catch
            throw:{?MODULE, Reason} ->
                throw({?MODULE, {invalid_list_element, I, Reason}})
        end
    end, lists:zip(lists:seq(1, length(List)), List));
parse_rule({tuple, Rules0}, Tuple) when (is_list(Rules0) orelse is_tuple(Rules0)), is_tuple(Tuple) ->
    Rules = case is_list(Rules0) of
        true -> Rules0;
        false -> tuple_to_list(Rules0)
    end,
    case length(Rules) =:= size(Tuple) of
        true -> ok;
        false -> throw({?MODULE, {invalid, {tuple, Rules0}, Tuple}})
    end,
    List = lists:zip(Rules, tuple_to_list(Tuple)),
    list_to_tuple(lists:map(fun({I, {Rule, Element}}) ->
        try parse_rule(Rule, Element) catch
            throw:{?MODULE, Reason} ->
                throw({?MODULE, {invalid_tuple_element, I, Reason}})
        end
    end, lists:zip(lists:seq(1, length(List)), List)));
parse_rule({optnl, Rule}, {value, Value}) ->
    try {value, parse_rule(Rule, Value)} catch
        throw:{?MODULE, Reason} ->
            throw({?MODULE, {invalid_optnl_value, Reason}})
    end;
parse_rule({optnl, _}, none) ->
    none;
parse_rule(any, Term) ->
    Term;
parse_rule(Rule, Invalid) ->
    throw({?MODULE, {invalid, Rule, Invalid}}).

lookup_map(Key, Map) ->
    case klsn_map:lookup([Key], Map) of
        none ->
            klsn_map:lookup([klsn_binstr:from_any(Key)], Map);
        Other ->
            Other
    end.

