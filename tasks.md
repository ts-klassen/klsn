## Story

As: codex erlang lib maintainer  
I want: JSON Schema to klsn_rule generator  
So that: I can use it to convert JSON -> erlang term and erlang term -> JSON

Fundamental idea: parse JSON Schema into klsn_rule terms, use struct_rule to map JSON objects into atom-keyed maps while dropping extras, and rely on klsn_rule normalization to round-trip between JSON maps and Erlang terms.

Done definition: we have a test that generates JSON map -> erlang term and erlang term -> JSON map for all schemas in `test/klsn_rule_generator_SUITE_data/codex-app-server-schema-0.98.0/`, and it passes.

## Checklist

- [ ] Confirm target semantics and JSON handling
  - [ ] Define JSON map key type and term map key type (binary in, atom out)
  - [ ] Lock normalization behavior (string to integer, enum normalization) and extra field policy
  - [ ] Decide how to treat format hints (e.g., int64) and boolean schemas
- [ ] Build schema resolution and inventory
  - [ ] Implement local $ref resolution (JSON Pointer into definitions)
  - [ ] Add registry/caching for named schemas and cycle detection
  - [ ] Inventory codex schemas for used features and edge cases
- [ ] Implement minimal generator (integer-only milestone)
  - [ ] Create generator entrypoint that reads a schema and emits rule terms
  - [ ] Support type=integer and object with one integer property
  - [ ] Add tiny golden tests for JSON to term and term to JSON
- [ ] Expand rule mapping to cover codex needs
  - [ ] Add primitives: string, boolean, number/float, null, enum, const, default
  - [ ] Add arrays and objects: items, properties, required, additionalProperties policy
  - [ ] Add combinators: anyOf, allOf, oneOf (custom exact-one rule)
- [ ] Validate on codex schema set
  - [ ] Run generator across codex schema files and resolve errors
  - [ ] Add targeted tests for tricky schemas (unions, nested objects, optional fields)
  - [ ] Document supported subset and known gaps

## Description

### Confirm target semantics and JSON handling

#### Define JSON map key type and term map key type (binary in, atom out)
Establish the input and output representations so generated rules can normalize JSON maps with binary keys into atom-keyed maps via struct_rule and convert back safely.

#### Lock normalization behavior (string to integer, enum normalization) and extra field policy
Keep klsn_rule normalization behavior and confirm that struct_rule drops extra properties to avoid ambiguity later.

#### Decide how to treat format hints (e.g., int64) and boolean schemas
Clarify whether format is ignored or enforced and how true/false schemas map (term vs always-reject).

### Build schema resolution and inventory

#### Implement local $ref resolution (JSON Pointer into definitions)
Support #/definitions/... so schemas can reuse shared definitions without manual inlining.

#### Add registry/caching for named schemas and cycle detection
Generate stable rule references and fail fast on unsupported recursion.

#### Inventory codex schemas for used features and edge cases
Scan the codex schema set to confirm which JSON Schema keywords are required.

### Implement minimal generator (integer-only milestone)

#### Create generator entrypoint that reads a schema and emits rule terms
Provide a basic pipeline from schema JSON into a rule term tree.

#### Support type=integer and object with one integer property
Use struct_rule for the object and integer for the field to validate JSON to term and term to JSON.

#### Add tiny golden tests for JSON to term and term to JSON
Lock the expected conversions for the first milestone.

### Expand rule mapping to cover codex needs

#### Add primitives: string, boolean, number/float, null, enum, const, default
Map these keywords to klsn_rule primitives and small helpers.

#### Add arrays and objects: items, properties, required, additionalProperties policy
Use list and struct rules, preserving your extra-fields-ignored policy.

#### Add combinators: anyOf, allOf, oneOf (custom exact-one rule)
Implement oneOf with a custom rule that rejects when more than one branch matches.

### Validate on codex schema set

#### Run generator across codex schema files and resolve errors
Iterate until all codex schemas generate rules successfully.

#### Add targeted tests for tricky schemas (unions, nested objects, optional fields)
Cover the areas most likely to break normalization or disambiguation.

#### Document supported subset and known gaps
Make the final behavior explicit for future schema upgrades.
