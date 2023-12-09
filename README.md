klsn
=====

An OTP library

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
