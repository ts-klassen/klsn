# Erlang Coding Conventions

## Keep changes narrowly scoped

- For a feature addition or bug fix, make the smallest possible diff that completes the request.
- Do not change existing code solely to bring it into compliance with these conventions, even when the existing code violates them.
- Refactor only when explicitly requested. Submit the refactoring as a single Change Set with no behavior changes.

Apply every rule below to Erlang code introduced or modified for the requested task.

## Name and format code

- Match each filename to its module name.
- Use `lower_snake_case` for filenames, modules, functions, types, and atoms. Allow the `_SUITE` suffix for Common Test modules and names required by OTP or external formats and protocols.
- Use `UpperCamelCase` for variables and `UPPER_SNAKE_CASE` for non-constant preprocessor macros.
- Use `_Name` or the wildcard `_` only where a pattern or callback signature requires an ignored value. Never write `_ = expression()` merely to discard a result; call `expression()` directly.
- Indent with four spaces. Never use tabs or leave trailing whitespace.
- Put spaces around match and map operators: `Value = Pattern`, `Key => Value`, and `Key := Value`.
- Keep a short, simple collection on one line. Format a multiline list, map, export, or argument sequence with the comma at the start of each item after the first. Preserve the format of generated code:

```erlang
-export([
        get/1
      , get/2
      , lookup/1
    ]).

Result = #{
    first => First
  , second => Second
}.
```

## Order declarations

Use this module-level order, omitting sections that do not apply:

1. `-module`
2. `-behaviour`
3. `-include` and `-include_lib`
4. `-define`
5. public `-export`
6. `-export_type`
7. callback or package-private `-export`
8. `-type` and `-opaque`
9. functions

Separate public exports from callback or package-private exports. Label the latter section when its purpose is not obvious. Never use `export_all`.

## Define constants sparingly

- Put constants in local variables or preprocessor macros. Do not introduce dedicated const modules or zero-arity functions solely to return constants.
- Prefer local variables. Use a macro when sharing a constant or when preprocessor semantics are required; do not replace ordinary code with macros unnecessarily.
- Give semantically different constants separate names even when their current literal values are equal.

## Specify every function and type precisely

- Put one `-spec` immediately before every function, whether public, private, or a behaviour callback. Put it before the first clause of a multi-clause function.
- Export every type referenced by another module with `-export_type`.
- Define structured data with precise map types. Mark required keys with `:=` and optional keys with `=>`.
- Do not use the broad types `map()` or `#{}`. Permit them only in a genuine generic utility that intentionally accepts every map.
- Do not use `list()`. Specify the element type, such as `[entry()]`. Permit `list()` only in a genuine generic utility that intentionally accepts every list.
- Do not use `atom()`. Define the actual atom union. Permit `atom()` only in a genuine generic utility that intentionally accepts every atom.
- Do not use `any()` or `term()`. Define the actual value union. Permit these types only in a genuine generic utility that intentionally accepts every Erlang term.
- Do not use `dynamic()` under any circumstances.
- Prefer a map for structured values and OTP state.
- Do not use records in repository APIs. When a standard or external library requires a record, contain it behind a wrapper.

## Represent optional values consistently

- Represent an optional `Value` as `klsn:optnl(ValueType)`, whose only values are `{value, Value}` and `none`.
- Preserve `klsn:'maybe'/1` as a compatibility alias, but use `klsn:optnl/1` in new or modified type references.
- Model an optional map field as an optional key. Prefer `#{key => ValueType}` to `#{key := klsn:optnl(ValueType)}`.
- Use `klsn_map:filter/1` to convert `maps:map(A, klsn:optnl(B))` to `maps:map(A, B)`. It drops entries whose value is `none` and unwraps `{value, Value}` entries.
- Never represent an optional value as `Value | none` or with another ad hoc sentinel.
- Do not use `undefined` or `nil` internally. Accept one only when a standard or external library requires it, then convert it to `klsn:optnl/1` at the boundary.
- Provide lookup APIs with these meanings:
  - `lookup(Key) -> klsn:optnl(ValueType)` never raises for absence.
  - `get(Key) -> ValueType` raises an error for absence.
  - `get(Key, Default) -> ValueType` returns `Default` for absence.
- Use `klsn_map` for nested maps.
- Avoid `klsn_obj` for nested objects because of its cost. Use it only when a measured or unavoidable requirement justifies it.

## Control flow and transformations

- Do not use `if`. Use `case` or function-clause pattern matching.
- Do not use list comprehensions. Use `lists:map`.
- Prefer a short semantic variable name for every transformation stage when one exists.
- Numeric stage suffixes may use any numbers. A progression such as `Value0`, `Value10`, `Value20`, and `Value30` is convenient because it leaves room to insert intermediate stages later. Use `Value` for the final form. Apply the same convention to state transitions, such as `State0` and `State`.
- When throwing, namespace the value with the current module, for example `throw({?MODULE, Reason})`.

## Avoid unnecessary helper functions

- Keep an operation at its use site unless multiple current call sites genuinely need the same operation.
- When code attempts to call a nonexistent helper, do not create the helper merely to resolve the error. Perform the operation directly at the use site.
- Create a shared, utility, or helper function only when an actual current need for sharing exists. Potential future reuse or broad applicability does not make an operation shared when it has only one call site.
- Prefer semantically independent pure functions for shared, utility, or helper code and cover them with EUnit tests. Allow side effects when the operation requires them and cover that behavior with Common Test.
- Never create a new unexported function with fewer than two ordinary call sites unless it meets one of the narrow structural exceptions below.

### Allow narrow structural exceptions

Treat these functions as structural code rather than shared, utility, or helper functions. Permit such an unexported function only when it independently meets one of these conditions:

- Allow a pure schema, rule, validator, encoder, decoder, normalizer, or denormalizer that corresponds one-to-one with an exported type or opaque. This includes a `klsn_rule` builder such as `*_rule/0` and pure named custom callbacks used only by that rule. Make the relationship clear from the name and spec.
- Allow a directly or mutually recursive worker that implements a loop or state machine, including a recursive `receive` loop. Treat recursive references as structural use. Do not add a separate single-use wrapper whose only purpose is to supply initial arguments.
- Allow an entry point or callback invoked by OTP, the compiler, a test runner, a framework, or a DSL instead of by an ordinary local call. This includes EUnit `*_test/0` and `*_test_/0` functions. Use a named callback only when the callback contract requires it or a multi-clause anonymous function would obscure the contract; passing a function as `fun Name/Arity` alone is not sufficient.
- Allow a function whose boundary owns a complete resource, concurrency, or transaction lifetime expressed by `try ... after`, a `receive` loop, a lock or monitor lifetime, or commit and rollback handling. Keep acquisition, use, cleanup, and failure handling within that boundary. Do not use this exception to split arbitrary workflow stages.
- Allow a pure multi-clause function that implements a complete closed-domain mapping or grammar production. Do not use this exception for a sequential block extracted only to shorten its caller.

Cover every pure production function permitted by an exception with EUnit tests, including every clause of a closed-domain mapping. Cover every side-effecting production function permitted by an exception with Common Test. If unsure whether an exception is pure, use Common Test.

Do not grant an exception merely because a function is long, a name seems descriptive, future reuse is possible, applicability is broad, or the function is passed to a higher-order API.

## Validate and normalize boundaries

- Validate and normalize external input at the boundary before domain processing.
- Convert untrusted binary names to atoms only through an explicit allowlist or `binary_to_existing_atom/1`. Never create an atom from untrusted input.
- Use binaries for application, wire, JSON, path, and human-readable text. Convert to lists only at an API boundary that requires a list, or use iolists to avoid unnecessary copying when assembling output.
- Add `/utf8` to binary literals containing non-ASCII text.

## Raise errors at the source

- Prefer raising errors directly at the exact site where an error is detected. Allow precisely typed results when validation, recovery, or communication between processes requires them.
- Accept `{ok, _}` or `{error, _}` when a standard or external library mandates that shape, and adapt it at the boundary.

## Structure OTP code

- Declare the applicable OTP behaviour explicitly.
- For a single-instance `gen_server`, expose `start_link/0`, register it locally with `?MODULE`, and keep its state in a precisely typed map.
- Keep public APIs separate from callback and package-private exports and implementations.
- Follow behaviour-mandated return shapes.

## Write comments and headers sparingly

- Write comments in English.
- Explain reasons, constraints, or non-obvious consequences rather than restating the code.
- Avoid adding `.hrl` files. Prefer module functions and exported types.
- When a header is unavoidable, add an include guard, name macros in `UPPER_SNAKE_CASE`, and limit macros to genuinely shared constants or preprocessor patterns.

## Review before finishing

Review every changed `.erl` and `.hrl` file against these conventions. Pay particular attention to complete `-spec` coverage, precise types, optional-value representation, boundary validation, error source, and forbidden constructs.
