-module(klsn_obj).

-export([
        get/2
      , lookup/2
    ]).

-export_type([
        value/0
      , key/0
      , nth/0
      , obj/0
      , cmd/0
      , path/0
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
        {tuple, Key0} -> erlang:error(not_found, [[H|T], Map]);
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
    try lists:nth(Nth, List) catch
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
    try element(Nth, Tuple) catch
        error:badarg ->
            erlang:error(not_found, [[H|T], Tuple])
    end.


