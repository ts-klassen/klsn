# klsn

[![Erlang CI](https://github.com/ts-klassen/klsn/actions/workflows/erlang-tests.yml/badge.svg?branch=main)](https://github.com/ts-klassen/klsn/actions/workflows/erlang-tests.yml)
[Coverage report](https://ts-klassen.github.io/klsn/report/)
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

### `klsn_map:upsert/3`
```
-spec upsert(key(), term(), map()) -> map().
```
