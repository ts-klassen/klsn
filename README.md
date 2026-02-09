# klsn

[![Erlang CI](https://github.com/ts-klassen/klsn/actions/workflows/erlang-tests.yml/badge.svg?branch=main)](https://github.com/ts-klassen/klsn/actions/workflows/erlang-tests.yml)
[Coverage report](https://ts-klassen.github.io/klsn/cover/)
[API docs](https://ts-klassen.github.io/klsn/edoc/)



An OTP library

Erlang library for ts-klassen projects.

Used by
- [ts-klassen/jobpq](https://github.com/ts-klassen/jobpq) job priority queue
- [ts-klassen/ghwhk](https://github.com/ts-klassen/ghwhk) Github app webhook for erlang
- [ts-klassen/gpte](https://github.com/ts-klassen/gpte) Openai ChatGPT for Erlang

Build
-----

    $ rebar3 compile

Rebar3 deps
-----------
`rebar.config`
```
{deps, [
    {klsn, {git, "https://github.com/ts-klassen/klsn.git", {tag, "1.0.0"}}}
]}.
```

Rules
-----
klsn_rule provides validation and normalization rules for common Erlang types.
Use `klsn_rule:validate(Input, Rule)` when you want an exception on invalid input, or
`klsn_rule:normalize(Input, Rule)` when you want a normalized value back.

Example
```
ok = klsn_rule:validate(42, integer),
Value = klsn_rule:normalize([<<"1">>, <<"2">>], {list, integer}).
```

Custom rules can be provided using `{custom, Name, Fun, Param}`, where `Fun` has
the signature `fun((Input, Param, State) -> klsn_rule:result())`.

Rule annotations
----------------
Include the header to enable the parse transform:

```
-include_lib("klsn/include/klsn_rule_annotation.hrl").
```

Annotate the next function with input and/or output rules:

```
-klsn_input_rule([integer, float]).
-klsn_output_rule(integer).
halve_even_or_float(Integer, Float) ->
    case Integer rem 2 of
        0 -> Integer div 2;
        1 -> Float
    end.
```

Notes:
- `-klsn_input_rule/1` takes a list with one rule per argument. Use `[]` for
  a zero-arity function.
- `-klsn_output_rule/1` takes a single rule.
- Rules are evaluated via `klsn_rule:eval/3` with `State = #{}`, so normalized values are accepted.
- Invalid inputs raise `erlang:error({klsn_input_rule, {Mod, Fun, Args}, Index, Reason})`.
- Invalid outputs raise `erlang:error({klsn_output_rule, {Mod, Fun, Args}, Output, Reason})`.
- The original function is renamed to `__klsn_rule_annotation__orig__<name>`,
  which may appear in stack traces.

Type
----

### `klsn:maybe/1`
```
-type maybe(Value) :: {value, Value} | none.
```

Function
--------

### `klsn_map:lookup/2`
```
-spec lookup(key(), map()) -> klsn:maybe(term()).
```

### `klsn_map:exists/2`
```
-spec exists(key(), map()) -> boolean().
```

### `klsn_map:upsert/3`
```
-spec upsert(key(), term(), map()) -> map().
```
