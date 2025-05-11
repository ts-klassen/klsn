-module(klsn_obj).

-export([
        get/2
      , lookup/2
      , find/2
      , crud/3
    ]).

-export_type([
        value/0
      , key/0
      , nth/0
      , obj/0
      , cmd/0
      , path/0
      , find_fun/0
    ]).

-type value() :: term().
-type key() :: term().
-type nth() :: pos_integer().
-type obj() :: value()
             | lists:list(obj())
             | tuple() % {}, {obj()}, {obj(), obj()}, ...
             | maps:map(key(), obj())
             .

-type cmd() :: {raw, nth() | key()}
             | {map, key()} | {m, key()}
             | {list, nth()} | {l, nth()}
             | {tuple, nth()} | {t, nth()}
             | nth() | key()
             .

-type path() :: [cmd()].

-type short_path() :: [{m, key()} | {l|t, nth()}].

-type find_fun() :: fun((value())->boolean())
                  | fun((value(), short_path())->boolean())
                  .

-type crud_fun() :: fun((klsn:maybe(value()))->klsn:maybe(value())).

-spec lookup(path(), obj()) -> klsn:maybe(value()).
lookup(Path, Obj) ->
    try get(Path, Obj) of
        Value ->
            {value, Value}
    catch
        error:not_found ->
            none
    end.

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

-spec crud(path(), crud_fun(), obj()) -> obj().
crud(Path, CRUDFun, Obj) ->
    {PathLeft, MaybeValue, History} = try crud_history(Path, Obj, []) catch
        throw:{?MODULE, Args} ->
            Args
    end,
    MaybeUpdatedValue = CRUDFun(MaybeValue),
    crud_build(lists:reverse(PathLeft), MaybeUpdatedValue, History).


-spec crud_history(
        path(), obj(), [{short_path(), obj()}]
    ) -> {path(), klsn:maybe(value()), [{short_path(), obj()}]}.
crud_history([], Value, History) ->
    {[], Value, History};
crud_history([H|T], Map, History) when is_map(Map) ->
    Key = case H of
        {raw, Key0} -> Key0;
        {map, Key0} -> Key0;
        {m, Key0} -> Key0;
        {list, _} -> erlang:throw({?MODULE, {[H|T], Map, History}});
        {l, _} -> erlang:throw({?MODULE, {[H|T], Map, History}});
        {tuple, _} -> erlang:throw({?MODULE, {[H|T], Map, History}});
        {t, _} -> erlang:throw({?MODULE, {[H|T], Map, History}});
        Key0 -> Key0
    end,
    case maps:find(Key, Map) of
        {ok, Value} ->
            crud_history(T, Value, [{{m,Key},Map}|History]);
        error ->
            {[{m,Key}|T], Map, History}
    end;
crud_history([H|T], List, History) when is_list(List) ->
    Nth = case H of
        {raw, Key0} when is_integer(Key0), (Key0 > 0) -> Key0;
        {map, _} -> erlang:throw({?MODULE, {[H|T], List, History}});
        {m, _} -> erlang:throw({?MODULE, {[H|T], List, History}});
        {list, Key0} when is_integer(Key0), (Key0 > 0) -> Key0;
        {l, Key0} when is_integer(Key0), (Key0 > 0) -> Key0;
        {tuple, _} -> erlang:throw({?MODULE, {[H|T], List, History}});
        {t, _} -> erlang:throw({?MODULE, {[H|T], List, History}});
        Key0 when is_integer(Key0), (Key0 > 0) -> Key0;
        _ -> erlang:throw({?MODULE, {[H|T], List, History}})
    end,
    try lists:nth(Nth, List) of
        Value ->
            crud_history(T, Value, [{{l,Nth},List}|History])
    catch
        error:function_clause ->
            {[{l,Nth}|T], List, History}
    end;
crud_history([H|T], Tuple, History) when is_tuple(Tuple) ->
    Nth = case H of
        {raw, Key0} when is_integer(Key0), (Key0 > 0) -> Key0;
        {map, _} -> erlang:throw({?MODULE, {[H|T], Tuple, History}});
        {m, _} -> erlang:throw({?MODULE, {[H|T], Tuple, History}});
        {list, _} -> erlang:throw({?MODULE, {[H|T], Tuple, History}});
        {l, _} -> erlang:throw({?MODULE, {[H|T], Tuple, History}});
        {tuple, Key0} when is_integer(Key0), (Key0 > 0) -> Key0;
        {t, Key0} when is_integer(Key0), (Key0 > 0) -> Key0;
        Key0 when is_integer(Key0), (Key0 > 0) -> Key0;
        _ -> erlang:throw({?MODULE, {[H|T], Tuple, History}})
    end,
    try element(Nth, Tuple) of
        Value ->
            crud_history(T, Value, [{{t,Nth},Tuple}|History])
    catch
        error:badarg ->
            {[{t,Nth}|T], Tuple, History}
    end;
crud_history(Path, Value, History) ->
    {Path, Value, History}.


-spec crud_build(
        klsn:maybe(obj())
      , [{short_path(), obj()}]
    ) -> obj().
crud_build(none, []) ->
    nil;
crud_build({value, Obj}, []) ->
    Obj;
crud_build(none, [{{m,Key},Map}|Tail]) when is_map(Map) ->
    crud_build({value, maps:remove(Key, Map)}, Tail);
crud_build(none, [{{l,Nth},List}|Tail]) when is_list(List) ->
    crud_build({value, lists:delete(Nth, List)}, Tail);
crud_build(none, [{{t,Nth},Tuple}|Tail]) when is_tuple(Tuple) ->
    List = tuple_to_list(Tuple),
    crud_build({value, list_to_tuple(lists:delete(Nth, List))}, Tail);
crud_build({value, Value}, [{{m,Key},Map}|Tail]) when is_map(Map) ->
    crud_build({value, Map#{ Key => Value }}, Tail);
crud_build({value, Value}, [{{l,Nth},List}|Tail]) when is_list(List) ->
    crud_build({value, replace_nth(Nth, Value, List)}, Tail);
crud_build({value, Value}, [{{t,Nth},Tuple}|Tail]) when is_tuple(Tuple) ->
    crud_build({value, replace_nth(Nth, Value, Tuple)}, Tail).

-spec crud_build(
        Reversed::path()
      , klsn:maybe(value())
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



