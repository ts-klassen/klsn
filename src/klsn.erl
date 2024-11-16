-module(klsn).

-export([
    ]).

-export_type([
        maybe/1
      , binstr/0
    ]).

-type maybe(Value) :: {value, Value} | none.

-type binstr() :: klsn_binstr:binstr().

