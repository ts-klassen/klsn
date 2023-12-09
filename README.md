klsn
=====

An OTP library

Build
-----

    $ rebar3 compile

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
