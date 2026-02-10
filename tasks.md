## Story

As: codex erlang lib maintainer  
I want: JSON Schema to klsn_rule generator  
So that: I can use it to convert JSON -> erlang term and erlang term -> JSON

Fundamental idea: parse JSON Schema into klsn_rule terms, use struct_rule to map JSON objects into atom-keyed maps while dropping extras, and rely on klsn_rule normalization to round-trip between JSON maps and Erlang terms.

Done definition: we have a test that generates JSON map -> erlang term and erlang term -> JSON map for all schemas in `test/klsn_rule_generator_SUITE_data/codex-app-server-schema-0.98.0/`, and it passes.

## Checklist

- [x] Confirm target semantics and JSON handling
  - [x] Define JSON map key type and term map key type (binary in, atom out)
  - [x] Lock normalization behavior (string to integer, enum normalization) and extra field policy
  - [x] Decide how to treat format hints (e.g., int64) and boolean schemas
- [x] Build schema resolution
  - [x] Implement local $ref resolution (JSON Pointer into definitions)
- [x] Implement minimal generator (integer-only milestone)
  - [x] Create generator entrypoint that reads a schema and emits rule terms
  - [x] Support type=integer and object with one integer property
  - [x] Add tiny golden tests for JSON to term and term to JSON
- [x] Expand rule mapping to cover codex needs
  - [x] Add primitives: string, boolean, number/float, null, enum, const, default
  - [x] Add arrays and objects: items, properties, required, additionalProperties policy
  - [x] Add combinators: anyOf, allOf, oneOf (custom exact-one rule)
- [ ] Validate on codex schema set
  - [ ] Run generator across codex schema files and resolve errors
  - [ ] Add targeted tests for tricky schemas (unions, nested objects, optional fields)
  - [ ] Document supported subset and known gaps
  - [ ] Add per-schema `.term` expected rule files for the remaining codex schema JSON (including ClientRequest.json, ServerRequest.json, and codex_app_server_protocol.schemas.json)
    - [ ] ApplyPatchApprovalParams.term
    - [ ] ApplyPatchApprovalResponse.term
    - [ ] ChatgptAuthTokensRefreshParams.term
    - [ ] ChatgptAuthTokensRefreshResponse.term
    - [ ] ClientNotification.term
    - [ ] ClientRequest.term
    - [ ] CommandExecutionRequestApprovalParams.term
    - [ ] CommandExecutionRequestApprovalResponse.term
    - [ ] DynamicToolCallParams.term
    - [ ] DynamicToolCallResponse.term
    - [ ] EventMsg.term
    - [ ] ExecCommandApprovalParams.term
    - [ ] ExecCommandApprovalResponse.term
    - [ ] FileChangeRequestApprovalParams.term
    - [ ] FileChangeRequestApprovalResponse.term
    - [ ] FuzzyFileSearchParams.term
    - [ ] FuzzyFileSearchResponse.term
    - [ ] JSONRPCError.term
    - [ ] JSONRPCErrorError.term
    - [ ] JSONRPCMessage.term
    - [x] JSONRPCNotification.term
    - [x] JSONRPCRequest.term
    - [ ] JSONRPCResponse.term
    - [x] RequestId.term
    - [ ] ServerNotification.term
    - [ ] ServerRequest.term
    - [ ] ToolRequestUserInputParams.term
    - [ ] ToolRequestUserInputResponse.term
    - [ ] codex_app_server_protocol.schemas.term
    - [ ] v1/AddConversationListenerParams.term
    - [ ] v1/AddConversationSubscriptionResponse.term
    - [ ] v1/ArchiveConversationParams.term
    - [ ] v1/ArchiveConversationResponse.term
    - [ ] v1/AuthStatusChangeNotification.term
    - [ ] v1/CancelLoginChatGptParams.term
    - [ ] v1/CancelLoginChatGptResponse.term
    - [ ] v1/ExecOneOffCommandParams.term
    - [ ] v1/ExecOneOffCommandResponse.term
    - [ ] v1/ForkConversationParams.term
    - [ ] v1/ForkConversationResponse.term
    - [ ] v1/GetAuthStatusParams.term
    - [ ] v1/GetAuthStatusResponse.term
    - [ ] v1/GetConversationSummaryParams.term
    - [ ] v1/GetConversationSummaryResponse.term
    - [ ] v1/GetUserAgentResponse.term
    - [ ] v1/GetUserSavedConfigResponse.term
    - [ ] v1/GitDiffToRemoteParams.term
    - [ ] v1/GitDiffToRemoteResponse.term
    - [ ] v1/InitializeParams.term
    - [ ] v1/InitializeResponse.term
    - [ ] v1/InterruptConversationParams.term
    - [ ] v1/InterruptConversationResponse.term
    - [ ] v1/ListConversationsParams.term
    - [ ] v1/ListConversationsResponse.term
    - [ ] v1/LoginApiKeyParams.term
    - [ ] v1/LoginApiKeyResponse.term
    - [ ] v1/LoginChatGptCompleteNotification.term
    - [ ] v1/LoginChatGptResponse.term
    - [ ] v1/LogoutChatGptResponse.term
    - [ ] v1/NewConversationParams.term
    - [ ] v1/NewConversationResponse.term
    - [ ] v1/RemoveConversationListenerParams.term
    - [ ] v1/RemoveConversationSubscriptionResponse.term
    - [ ] v1/ResumeConversationParams.term
    - [ ] v1/ResumeConversationResponse.term
    - [ ] v1/SendUserMessageParams.term
    - [ ] v1/SendUserMessageResponse.term
    - [ ] v1/SendUserTurnParams.term
    - [ ] v1/SendUserTurnResponse.term
    - [ ] v1/SessionConfiguredNotification.term
    - [ ] v1/SetDefaultModelParams.term
    - [ ] v1/SetDefaultModelResponse.term
    - [ ] v1/UserInfoResponse.term
    - [ ] v2/AccountLoginCompletedNotification.term
    - [ ] v2/AccountRateLimitsUpdatedNotification.term
    - [ ] v2/AccountUpdatedNotification.term
    - [ ] v2/AgentMessageDeltaNotification.term
    - [ ] v2/AppsListParams.term
    - [ ] v2/AppsListResponse.term
    - [ ] v2/CancelLoginAccountParams.term
    - [ ] v2/CancelLoginAccountResponse.term
    - [ ] v2/CommandExecParams.term
    - [ ] v2/CommandExecResponse.term
    - [ ] v2/CommandExecutionOutputDeltaNotification.term
    - [ ] v2/ConfigBatchWriteParams.term
    - [ ] v2/ConfigReadParams.term
    - [ ] v2/ConfigReadResponse.term
    - [ ] v2/ConfigRequirementsReadResponse.term
    - [ ] v2/ConfigValueWriteParams.term
    - [ ] v2/ConfigWarningNotification.term
    - [ ] v2/ConfigWriteResponse.term
    - [ ] v2/ContextCompactedNotification.term
    - [ ] v2/DeprecationNoticeNotification.term
    - [ ] v2/ErrorNotification.term
    - [ ] v2/FeedbackUploadParams.term
    - [ ] v2/FeedbackUploadResponse.term
    - [ ] v2/FileChangeOutputDeltaNotification.term
    - [ ] v2/GetAccountParams.term
    - [ ] v2/GetAccountRateLimitsResponse.term
    - [ ] v2/GetAccountResponse.term
    - [ ] v2/ItemCompletedNotification.term
    - [ ] v2/ItemStartedNotification.term
    - [ ] v2/ListMcpServerStatusParams.term
    - [ ] v2/ListMcpServerStatusResponse.term
    - [ ] v2/LoginAccountParams.term
    - [ ] v2/LoginAccountResponse.term
    - [ ] v2/LogoutAccountResponse.term
    - [ ] v2/McpServerOauthLoginCompletedNotification.term
    - [ ] v2/McpServerOauthLoginParams.term
    - [ ] v2/McpServerOauthLoginResponse.term
    - [ ] v2/McpServerRefreshResponse.term
    - [ ] v2/McpToolCallProgressNotification.term
    - [ ] v2/ModelListParams.term
    - [ ] v2/ModelListResponse.term
    - [ ] v2/PlanDeltaNotification.term
    - [ ] v2/RawResponseItemCompletedNotification.term
    - [ ] v2/ReasoningSummaryPartAddedNotification.term
    - [ ] v2/ReasoningSummaryTextDeltaNotification.term
    - [ ] v2/ReasoningTextDeltaNotification.term
    - [ ] v2/ReviewStartParams.term
    - [ ] v2/ReviewStartResponse.term
    - [ ] v2/SkillsConfigWriteParams.term
    - [ ] v2/SkillsConfigWriteResponse.term
    - [ ] v2/SkillsListParams.term
    - [ ] v2/SkillsListResponse.term
    - [ ] v2/SkillsRemoteReadParams.term
    - [ ] v2/SkillsRemoteReadResponse.term
    - [ ] v2/SkillsRemoteWriteParams.term
    - [ ] v2/SkillsRemoteWriteResponse.term
    - [ ] v2/TerminalInteractionNotification.term
    - [ ] v2/ThreadArchiveParams.term
    - [ ] v2/ThreadArchiveResponse.term
    - [ ] v2/ThreadCompactStartParams.term
    - [ ] v2/ThreadCompactStartResponse.term
    - [ ] v2/ThreadForkParams.term
    - [ ] v2/ThreadForkResponse.term
    - [ ] v2/ThreadListParams.term
    - [ ] v2/ThreadListResponse.term
    - [ ] v2/ThreadLoadedListParams.term
    - [ ] v2/ThreadLoadedListResponse.term
    - [ ] v2/ThreadNameUpdatedNotification.term
    - [ ] v2/ThreadReadParams.term
    - [ ] v2/ThreadReadResponse.term
    - [ ] v2/ThreadResumeParams.term
    - [ ] v2/ThreadResumeResponse.term
    - [ ] v2/ThreadRollbackParams.term
    - [ ] v2/ThreadRollbackResponse.term
    - [ ] v2/ThreadSetNameParams.term
    - [ ] v2/ThreadSetNameResponse.term
    - [ ] v2/ThreadStartParams.term
    - [ ] v2/ThreadStartResponse.term
    - [ ] v2/ThreadStartedNotification.term
    - [ ] v2/ThreadTokenUsageUpdatedNotification.term
    - [ ] v2/ThreadUnarchiveParams.term
    - [ ] v2/ThreadUnarchiveResponse.term
    - [ ] v2/TurnCompletedNotification.term
    - [ ] v2/TurnDiffUpdatedNotification.term
    - [ ] v2/TurnInterruptParams.term
    - [ ] v2/TurnInterruptResponse.term
    - [ ] v2/TurnPlanUpdatedNotification.term
    - [ ] v2/TurnStartParams.term
    - [ ] v2/TurnStartResponse.term
    - [ ] v2/TurnStartedNotification.term
    - [ ] v2/WindowsWorldWritableWarningNotification.term
  - [ ] Make klsn_rule_generator_SUITE strict: require .term for every schema (do this after the per-schema list is complete)

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

#### Add per-schema .term expected rule files (manual only)
For each schema JSON, create a matching .term file in the same relative path under
`test/klsn_rule_generator_SUITE_data/codex-app-server-schema-0.98.0/`. Write the
expected rule by hand based on the schema and generator behavior. Do not auto-generate
expected files (no scripts, no generator outputs).

When you add a .term file, append its relative path to
`test/klsn_rule_generator_SUITE_data/expected_terms.list` to include it in the CT run.

Always run this suite after adding or updating any expected .term file:
`rebar3 ct --suite klsn_rule_generator_SUITE`
