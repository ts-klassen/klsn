-module(klsn).

-export([
    ]).

-export_type([
        maybe/1
      , binstr/0
    ]).

%% A Maybe value — either {value, V} when a result is present or the
%% atom none when it is absent.
-type maybe(Value) :: {value, Value} | none.

%% UTF-8 binary string alias re-exported from klsn_binstr for
%% convenience.
-type binstr() :: klsn_binstr:binstr().

