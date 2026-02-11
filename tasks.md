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
    - [x] ApplyPatchApprovalParams.term
    - [x] ApplyPatchApprovalResponse.term
    - [x] ChatgptAuthTokensRefreshParams.term
    - [x] ChatgptAuthTokensRefreshResponse.term
    - [x] ClientNotification.term
    - [x] ClientRequest.term
    - [x] CommandExecutionRequestApprovalParams.term
    - [x] CommandExecutionRequestApprovalResponse.term
    - [x] DynamicToolCallParams.term
    - [x] DynamicToolCallResponse.term
    - [x] EventMsg.term
    - [x] ExecCommandApprovalParams.term
    - [x] ExecCommandApprovalResponse.term
    - [x] FileChangeRequestApprovalParams.term
    - [x] FileChangeRequestApprovalResponse.term
    - [x] FuzzyFileSearchParams.term
    - [x] FuzzyFileSearchResponse.term
    - [x] JSONRPCError.term
    - [x] JSONRPCErrorError.term
    - [x] JSONRPCMessage.term
    - [x] JSONRPCNotification.term
    - [x] JSONRPCRequest.term
    - [x] JSONRPCResponse.term
    - [x] RequestId.term
    - [x] ServerNotification.term
    - [x] ServerRequest.term
    - [x] ToolRequestUserInputParams.term
    - [x] ToolRequestUserInputResponse.term
    - [-] codex_app_server_protocol.schemas.term
    - [x] v1/AddConversationListenerParams.term
    - [x] v1/AddConversationSubscriptionResponse.term
    - [x] v1/ArchiveConversationParams.term
    - [x] v1/ArchiveConversationResponse.term
    - [x] v1/AuthStatusChangeNotification.term
    - [x] v1/CancelLoginChatGptParams.term
    - [x] v1/CancelLoginChatGptResponse.term
    - [x] v1/ExecOneOffCommandParams.term
    - [x] v1/ExecOneOffCommandResponse.term
    - [x] v1/ForkConversationParams.term
    - [-] v1/ForkConversationResponse.term
    - [x] v1/GetAuthStatusParams.term
    - [x] v1/GetAuthStatusResponse.term
    - [x] v1/GetConversationSummaryParams.term
    - [x] v1/GetConversationSummaryResponse.term
    - [x] v1/GetUserAgentResponse.term
    - [x] v1/GetUserSavedConfigResponse.term
    - [x] v1/GitDiffToRemoteParams.term
    - [x] v1/GitDiffToRemoteResponse.term
    - [x] v1/InitializeParams.term
    - [x] v1/InitializeResponse.term
    - [x] v1/InterruptConversationParams.term
    - [x] v1/InterruptConversationResponse.term
    - [x] v1/ListConversationsParams.term
    - [x] v1/ListConversationsResponse.term
    - [x] v1/LoginApiKeyParams.term
    - [x] v1/LoginApiKeyResponse.term
    - [x] v1/LoginChatGptCompleteNotification.term
    - [x] v1/LoginChatGptResponse.term
    - [x] v1/LogoutChatGptResponse.term
    - [x] v1/NewConversationParams.term
    - [x] v1/NewConversationResponse.term
    - [x] v1/RemoveConversationListenerParams.term
    - [x] v1/RemoveConversationSubscriptionResponse.term
    - [x] v1/ResumeConversationParams.term
    - [-] v1/ResumeConversationResponse.term
    - [x] v1/SendUserMessageParams.term
    - [x] v1/SendUserMessageResponse.term
    - [x] v1/SendUserTurnParams.term
    - [x] v1/SendUserTurnResponse.term
    - [x] v1/SessionConfiguredNotification.term
    - [x] v1/SetDefaultModelParams.term
    - [x] v1/SetDefaultModelResponse.term
    - [x] v1/UserInfoResponse.term
    - [x] v2/AccountLoginCompletedNotification.term
    - [x] v2/AccountRateLimitsUpdatedNotification.term
    - [x] v2/AccountUpdatedNotification.term
    - [x] v2/AgentMessageDeltaNotification.term
    - [x] v2/AppsListParams.term
    - [x] v2/AppsListResponse.term
    - [x] v2/CancelLoginAccountParams.term
    - [x] v2/CancelLoginAccountResponse.term
    - [x] v2/CommandExecParams.term
    - [x] v2/CommandExecResponse.term
    - [x] v2/CommandExecutionOutputDeltaNotification.term
    - [x] v2/ConfigBatchWriteParams.term
    - [x] v2/ConfigReadParams.term
    - [x] v2/ConfigReadResponse.term
    - [x] v2/ConfigRequirementsReadResponse.term
    - [x] v2/ConfigValueWriteParams.term
    - [x] v2/ConfigWarningNotification.term
    - [x] v2/ConfigWriteResponse.term
    - [x] v2/ContextCompactedNotification.term
    - [x] v2/DeprecationNoticeNotification.term
    - [x] v2/ErrorNotification.term
    - [x] v2/FeedbackUploadParams.term
    - [x] v2/FeedbackUploadResponse.term
    - [x] v2/FileChangeOutputDeltaNotification.term
    - [x] v2/GetAccountParams.term
    - [x] v2/GetAccountRateLimitsResponse.term
    - [x] v2/GetAccountResponse.term
    - [x] v2/ItemCompletedNotification.term
    - [x] v2/ItemStartedNotification.term
    - [x] v2/ListMcpServerStatusParams.term
    - [x] v2/ListMcpServerStatusResponse.term
    - [x] v2/LoginAccountParams.term
    - [x] v2/LoginAccountResponse.term
    - [x] v2/LogoutAccountResponse.term
    - [x] v2/McpServerOauthLoginCompletedNotification.term
    - [x] v2/McpServerOauthLoginParams.term
    - [x] v2/McpServerOauthLoginResponse.term
    - [x] v2/McpServerRefreshResponse.term
    - [x] v2/McpToolCallProgressNotification.term
    - [x] v2/ModelListParams.term
    - [x] v2/ModelListResponse.term
    - [x] v2/PlanDeltaNotification.term
    - [x] v2/RawResponseItemCompletedNotification.term
    - [x] v2/ReasoningSummaryPartAddedNotification.term
    - [x] v2/ReasoningSummaryTextDeltaNotification.term
    - [x] v2/ReasoningTextDeltaNotification.term
    - [x] v2/ReviewStartParams.term
    - [-] v2/ReviewStartResponse.term
    - [x] v2/SkillsConfigWriteParams.term
    - [x] v2/SkillsConfigWriteResponse.term
    - [x] v2/SkillsListParams.term
    - [x] v2/SkillsListResponse.term
    - [x] v2/SkillsRemoteReadParams.term
    - [x] v2/SkillsRemoteReadResponse.term
    - [x] v2/SkillsRemoteWriteParams.term
    - [x] v2/SkillsRemoteWriteResponse.term
    - [x] v2/TerminalInteractionNotification.term
    - [x] v2/ThreadArchiveParams.term
    - [x] v2/ThreadArchiveResponse.term
    - [x] v2/ThreadCompactStartParams.term
    - [x] v2/ThreadCompactStartResponse.term
    - [x] v2/ThreadForkParams.term
    - [x] v2/ThreadForkResponse.term
    - [x] v2/ThreadListParams.term
    - [x] v2/ThreadListResponse.term
    - [x] v2/ThreadLoadedListParams.term
    - [x] v2/ThreadLoadedListResponse.term
    - [x] v2/ThreadNameUpdatedNotification.term
    - [x] v2/ThreadReadParams.term
    - [x] v2/ThreadReadResponse.term
    - [x] v2/ThreadResumeParams.term
    - [x] v2/ThreadResumeResponse.term
    - [x] v2/ThreadRollbackParams.term
    - [x] v2/ThreadRollbackResponse.term
    - [x] v2/ThreadSetNameParams.term
    - [x] v2/ThreadSetNameResponse.term
    - [x] v2/ThreadStartParams.term
    - [x] v2/ThreadStartResponse.term
    - [x] v2/ThreadStartedNotification.term
    - [x] v2/ThreadTokenUsageUpdatedNotification.term
    - [x] v2/ThreadUnarchiveParams.term
    - [x] v2/ThreadUnarchiveResponse.term
    - [x] v2/TurnCompletedNotification.term
    - [x] v2/TurnDiffUpdatedNotification.term
    - [x] v2/TurnInterruptParams.term
    - [x] v2/TurnInterruptResponse.term
    - [x] v2/TurnPlanUpdatedNotification.term
    - [x] v2/TurnStartParams.term
    - [x] v2/TurnStartResponse.term
    - [x] v2/TurnStartedNotification.term
    - [x] v2/WindowsWorldWritableWarningNotification.term
  - [x] Make klsn_rule_generator_SUITE strict: require .term for every schema (do this after the per-schema list is complete)

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
