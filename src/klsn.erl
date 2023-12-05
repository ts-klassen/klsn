-module(klsn).

-export([
    ]).

-export_type([
        maybe/1
    ]).

-type maybe(Value) :: {value, Value} | none.

