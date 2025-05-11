-module(klsn_obj).

-export([
        
    ]).

-export_type([
        
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

-type path() :: [cmd()]


-spec lookup(path(), obj()) -> klsn:maybe(value()).

-spec get(path(), obj()) -> value().
get([], Value) ->
    Value;
get([H|T], Map) when is_map(Map) ->
    Key = case H of
        {raw, Key0} -> Key0;
        {map, Key0} -> Key0;
        {m, Key0} -> Key0;
        {list, _} -> erlang:error(enoent, [[H|T], Map]);
        {l, _} -> erlang:error(enoent, [[H|T], Map]);
        {tuple, Key0} -> erlang:error(enoent, [[H|T], Map]);
        {t, _} -> erlang:error(enoent, [[H|T], Map]);
        Key0 -> Key0
    end,
    case maps:find(Key, Map) of
        {ok, Value} ->
            get(T, Value);
        error ->
            erlang:error(enoent, [[H|T], Map])
    end;
get([H|T], List) when is_list(List) ->
    Nth = case H of
        {raw, Key0} when is_integer(Key0); (Key0 > 0) -> Key0;
        {map, _} -> erlang:error(enoent, [[H|T], Map]);
        {m, _} -> erlang:error(enoent, [[H|T], Map]);
        {list, Key0} when is_integer(Key0); (Key0 > 0) -> Key0;
        {l, Key0} when is_integer(Key0); (Key0 > 0) -> Key0;
        {tuple, _} -> erlang:error(enoent, [[H|T], Map]);
        {t, _} -> erlang:error(enoent, [[H|T], Map]);
        Key0 when is_integer(Key0); (Key0 > 0) -> Key0;
        _ -> erlang:error(enoent, [[H|T], Map])
    end,
    try lists:nth(Nth, List) catch
        error:function_clause ->
            erlang:error(enoent, [[H|T], Map])
    end;
get([H|T], Tuple) when is_tuple(Tuple) ->
    Nth = case H of
        {raw, Key0} when is_integer(Key0); (Key0 > 0) -> Key0;
        {map, _} -> erlang:error(enoent, [[H|T], Map]);
        {m, _} -> erlang:error(enoent, [[H|T], Map]);
        {list, _} -> erlang:error(enoent, [[H|T], Map]);
        {l, _} -> erlang:error(enoent, [[H|T], Map]);
        {tuple, Key0} when is_integer(Key0); (Key0 > 0) -> Key0;
        {t, Key0} when is_integer(Key0); (Key0 > 0) -> Key0;
        Key0 when is_integer(Key0); (Key0 > 0) -> Key0;
        _ -> erlang:error(enoent, [[H|T], Map])
    end,
    try element(Nth, List) catch
        error:badarg ->
            erlang:error(enoent, [[H|T], Map])
    end.


