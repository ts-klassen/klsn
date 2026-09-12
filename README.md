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

`klsn_bwrap` uses Erlang `open_port/2` on Linux. It requires `bwrap`, Bash,
`kill`, and the GNU `cat`, `dd`, `env`, `mkdir`, `rm`, and `rmdir`
utilities on `PATH`.
Anonymous pipes connect stdin, stdout and stderr to the sandbox, including
when commands reopen those descriptors. A private directory under `TMPDIR`
(default `/tmp`) holds diagnostics and the caller's
environment snapshot; it is removed when the command finishes. Bash's job
status preserves the distinction between exit
codes and signals; output draining finishes even when descendants retain
the pipes. The transport needs no native extension or custom executable.

Streaming writes apply backpressure: `klsn_bwrap:send/2` waits without a
time limit until the native stdin buffer accepts the chunk. Use
`klsn_bwrap:send(Stream, Binary, Timeout)` to bound that wait in milliseconds,
or `infinity` for the existing behavior. `klsn_bwrap:send_eof(Stream, Timeout)`
also bounds the wait for preceding input before requesting stdin closure.
An application response timeout that starts after sending does not cover
these waits.

On a send timeout, the call raises `error:timeout` and terminates the whole
transport, discarding pending input. Open a new stream before sending again.
Some or all of the data may already have reached the sandbox; a timeout does
not prove that a request was never executed. Native cleanup continues in the
background, and already buffered bytes may still reach the sandbox during it.
This cancellation emits no stream completion message; if another
process owns the stream, it can monitor the handle's `exec_pid` for termination.
Previously delivered output remains in the owner's mailbox.

Rebar3 deps
-----------
`rebar.config`
```
{deps, [
    {klsn, {git, "https://github.com/ts-klassen/klsn.git", {tag, "1.27.0"}}}
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
