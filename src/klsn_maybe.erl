-module(klsn_maybe).

-export([
        get_value/1
      , get_value/2
    ]).

-export_type([
    ]).

-spec get_value({value, Value}) -> Value.
get_value({value, Value}) ->
    Value;
get_value(None) ->
    erlang:error(badarg, [None]).

-spec get_value
    ({value, Value}, Default::any()) -> Value;
    (none, Default) -> Default.
get_value({value, Value}, _Default) ->
    Value;
get_value(none, Default) ->
    Default;
get_value(Arg1, Arg2) ->
    erlang:error(badarg, [Arg1, Arg2]).

