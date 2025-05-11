-module(klsn_obj).

-export([
        get/2
      , lookup/2
      , find/2
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



