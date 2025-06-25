-module(klsn_map).

-export([
        lookup/2
      , get/2
      , get/3
      , upsert/3
      , remove/2
      , filter/1
      , invert/1
    ]).

-export_type([
        key/0
    ]).

%% A path into a nested map, represented as a list of keys. Example:
%% [foo, bar, baz] first looks up foo in the outer map, then bar in
%% the returned map, and finally baz.
-type key() :: [term()].


%% @doc
%% Fetch the value at Key inside Map or raise error:not_found when
%% the path cannot be resolved.
-spec get(key(), map()) -> term().
get(Key, Map) ->
    case lookup(Key, Map) of
        {value, Value} ->
            Value;
        none ->
            erlang:error(not_found, [Key, Map])
    end.

%% @doc
%% Same as get/2 but returns Default instead of throwing.
-spec get(key(), map(), term()) -> term().
get(Key, Map, Default) ->
    klsn_maybe:get_value(lookup(Key, Map), Default).

%% @doc
%% Variant of get/2 that never throws. Returns {value, V} when the
%% path exists or none otherwise.
-spec lookup(key(), map()) -> klsn:maybe(term()).
lookup([], Value) ->
    {value, Value};
lookup(_, Value) when not is_map(Value) ->
    none;
lookup([H|T], Map) ->
    case maps:find(H, Map) of
        {ok, Value} ->
            lookup(T, Value);
        error ->
            none
    end.

%% @doc
%% Insert or replace the element located at *Key* with *Value* inside
%% *Map*.  Missing intermediary maps are created on-the-fly.
-spec upsert(key(), term(), map()) -> map().
upsert(Key, Value, Map) ->
    upsert_(Key, Value, {value, Map}, [], []).
upsert_([], Value, _, [], []) ->
    Value;
upsert_([], Value, _, [{value, Map}|Maps], [Key|Keys]) ->
    upsert_([], Map#{Key=>Value}, none, Maps, Keys);
upsert_([], Value, _, [none|Maps], [Key|Keys]) ->
    upsert_([], #{Key=>Value}, none, Maps, Keys);
upsert_([H|T], Value, none, Maps, Keys) ->
    upsert_(T, Value, none, [none|Maps], [H|Keys]);
upsert_([H|T], Value, {value, Map}, Maps, Keys) ->
    Elem = lookup([H], Map),
    upsert_(T, Value, Elem, [{value, Map}|Maps], [H|Keys]).


%% @doc
%% TODO: (codex) Write a document.
-spec remove(key(), map()) -> map().
remove([], _Map) ->
    #{};
remove(Path, Map) ->
    [KeyToDelete|PathToKeepRev] = lists:reverse(Path),
    PathToKeep = lists:reverse(PathToKeepRev),
    case lookup(PathToKeep, Map) of
        {value, MapToDelete} when is_map(MapToDelete) ->
            upsert(PathToKeep, maps:remove(KeyToDelete, MapToDelete), Map);
        _ ->
            Map
    end.


%% @doc
%% Remove entries whose value is none and unwrap {value, V}.
-spec filter(maps:map(Key, klsn:maybe(Value))) -> maps:map(Key, Value).
filter(Map) ->
    maps:filtermap(fun
        (_, {value, Value}) ->
            {true, Value};
        (_, none) ->
            false
    end, Map).

%% @doc
%% Produce a new map where keys become values and vice-versa.
-spec invert(maps:map(Key, Value)) -> maps:map(Value, Key).
invert(Map) ->
    maps:from_list(lists:map(fun({Key, Value})->
        {Value, Key}
    end, maps:to_list(Map))).
