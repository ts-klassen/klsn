-module(klsn_maybe).

-export([
        get_value/1
      , get_value/2
      , filtermap/2
    ]).

%% A small helper module for working with the *Maybe* pattern
%% (`{value, V} | none') that is defined in `klsn' itself.  The functions
%% mirror common operations from the Haskell `Maybe` / Rust `Option`
%% family while keeping the interface idiomatic Erlang.

-export_type([
    ]).

%% @doc
%% Extract the wrapped value or raise `badarg` when *None* is supplied.
-spec get_value({value, Value}) -> Value.
get_value({value, Value}) ->
    Value;
get_value(None) ->
    erlang:error(badarg, [None]).

%% @doc
%% Return the contained value when present, otherwise return *Default*.
-spec get_value
    ({value, Value}, Default::any()) -> Value;
    (none, Default) -> Default.
get_value({value, Value}, _Default) ->
    Value;
get_value(none, Default) ->
    Default;
get_value(Arg1, Arg2) ->
    erlang:error(badarg, [Arg1, Arg2]).


%% @doc
%% Combine filtering and mapping in one pass.  When the callback returns
%% `{value, V}` the element is kept (and *V* becomes the new value);
%% returning `none` discards the element.
-spec filtermap(fun((any())->klsn:maybe(any())), list()) -> list();
               (fun((any(), any())->klsn:maybe(any())), map()) -> map().
filtermap(Fun, List) when is_list(List) ->
    lists:filtermap(fun(Val) ->
        case Fun(Val) of
            {value, Value} ->
                {true, Value};
            none ->
                false
        end
    end, List);
filtermap(Fun, Map) when is_map(Map) ->
    maps:filtermap(fun(Key, Val) ->
        case Fun(Key, Val) of
            {value, Value} ->
                {true, Value};
            none ->
                false
        end
    end, Map).
