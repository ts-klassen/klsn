-module(klsn_map).

-export([
        lookup/2
      , get/2
      , get/3
      , upsert/3
    ]).

-export_type([
        key/0
    ]).

-type key() :: [term()].


-spec get(key(), map()) -> term().
get(Key, Map) ->
    case lookup(Key, Map) of
        {value, Value} ->
            Value;
        none ->
            erlang:error(not_found, [Key, Map])
    end.

-spec get(key(), map(), term()) -> term().
get(Key, Map, Default) ->
    klsn_maybe:get_value(lookup(Key, Map), Default).

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
