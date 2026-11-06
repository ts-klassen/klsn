-module(klsn_rule_generator_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([
        all/0
      , schemas_match_expected/1
      , codex_157_common_constraints/1
      , codex_167_nested_choices/1
    ]).

-spec all() -> [schemas_match_expected
              | codex_157_common_constraints
              | codex_167_nested_choices
              ].
all() ->
    [
        schemas_match_expected
      , codex_157_common_constraints
      , codex_167_nested_choices
    ].

schemas_match_expected(Config) ->
    DataDir = ?config(data_dir, Config),
    SchemaRoot = filename:join(DataDir, "codex-app-server-schema-0.98.0"),
    SchemaPaths = schema_paths_(SchemaRoot),
    ExpectedPairs = expected_pairs_(SchemaRoot, SchemaPaths),
    Results = parallel_map_(fun(Pair) -> check_pair_(SchemaRoot, Pair) end, ExpectedPairs),
    case [Error || Error = {error, _RelPath, _} <- Results] of
        [] ->
            ok;
        Errors ->
            [{error, _RelPath, Detail} | _] = lists:keysort(2, Errors),
            ct:fail(Detail)
    end,
    ok.

expected_rules_(#{from_json := _, to_json := _} = Rules) ->
    Rules;
expected_rules_(Rule) ->
    #{from_json => Rule, to_json => Rule}.

schema_paths_(SchemaRoot) ->
    Paths = filelib:fold_files(
        SchemaRoot,
        ".*\\.json$",
        true,
        fun(Path, Acc) -> [Path | Acc] end,
        []
    ),
    lists:sort(Paths).

expected_pairs_(SchemaRoot, SchemaPaths) ->
    lists:map(fun(SchemaPath) ->
        ExpectedPath = filename:rootname(SchemaPath, ".json") ++ ".term",
        case filelib:is_file(ExpectedPath) of
            true ->
                {ExpectedPath, SchemaPath};
            false ->
                RelPath = relpath_(SchemaRoot, SchemaPath),
                ct:fail({missing_expected_file, RelPath})
        end
    end, SchemaPaths).

check_pair_(SchemaRoot, {ExpectedPath, SchemaPath}) ->
    RelPath = relpath_(SchemaRoot, SchemaPath),
    try
        {ok, [ExpectedRule]} = file:consult(ExpectedPath),
        {ok, Bin} = file:read_file(SchemaPath),
        Schema = jsone:decode(Bin),
        Actual = klsn_rule_generator:from_json_schema(Schema),
        Expected = expected_rules_(ExpectedRule),
        case Actual =:= Expected of
            true ->
                ok;
            false ->
                {error, RelPath, {schema_mismatch, RelPath, {expected, Expected}, {actual, Actual}}}
        end
    catch
        Class:Reason:Stack ->
            {error, RelPath, {schema_check_failed, RelPath, {Class, Reason, Stack}}}
    end.

parallel_map_(_Fun, []) ->
    [];
parallel_map_(Fun, Items) ->
    Total = length(Items),
    Workers = case erlang:system_info(schedulers_online) of
        N when N > 0 -> N;
        _ -> 1
    end,
    Concurrency = case Total < Workers of
        true -> Total;
        false -> Workers
    end,
    {Initial, Pending} = lists:split(Concurrency, Items),
    Parent = self(),
    Ref = make_ref(),
    lists:foreach(fun(Item) -> spawn_worker_(Fun, Ref, Parent, Item) end, Initial),
    collect_parallel_results_(Fun, Ref, Parent, Pending, Total, []).

collect_parallel_results_(_Fun, _Ref, _Parent, _Pending, 0, Acc) ->
    lists:reverse(Acc);
collect_parallel_results_(Fun, Ref, Parent, Pending, Remaining, Acc) ->
    receive
        {Ref, Result} ->
            case Pending of
                [Next | Rest] ->
                    spawn_worker_(Fun, Ref, Parent, Next),
                    collect_parallel_results_(Fun, Ref, Parent, Rest, Remaining - 1, [Result | Acc]);
                [] ->
                    collect_parallel_results_(Fun, Ref, Parent, [], Remaining - 1, [Result | Acc])
            end
    end.

spawn_worker_(Fun, Ref, Parent, Item) ->
    spawn(fun() ->
        Result = safe_apply_(Fun, Item),
        Parent ! {Ref, Result}
    end).

safe_apply_(Fun, Item) ->
    try Fun(Item)
    catch
        Class:Reason:Stack ->
            {error, unknown, {worker_crash, {Class, Reason, Stack}}}
    end.

relpath_(Base, Path) ->
    BaseParts = filename:split(filename:absname(Base)),
    PathParts = filename:split(filename:absname(Path)),
    case lists:prefix(BaseParts, PathParts) of
        true ->
            filename:join(lists:nthtail(length(BaseParts), PathParts));
        false ->
            Path
    end.

%% Exercise generated rules against unmodified real schema definitions, with
%% binary JSON keys/values on input and atom keys/enums on the Erlang side.
-spec codex_157_common_constraints(ct_suite:ct_config()) -> ok.
codex_157_common_constraints(Config) ->
    Root = filename:join(?config(data_dir, Config), <<"codex-app-server-schema-0.157.1">>),
    lists:foreach(fun({Name, Type, Fields}) ->
        Path = filename:join(Root, <<Name/binary, ".json">>),
        {ok, SchemaJson} = file:read_file(Path),
        Rules = klsn_rule_generator:from_json_schema(jsone:decode(SchemaJson)),
        lists:foreach(fun(Direction) ->
            Rule = maps:get(Direction, Rules),
            lists:foreach(fun(Field) ->
                Base = #{type => Type, Field => <<"example">>},
                lists:foreach(fun(Input) ->
                    Input = normalize_fixture_(Input, Rule, Direction)
                end, [Base, Base#{detail => high}, Base#{detail => null}]),
                reject_fixture_(maps:remove(type, Base), Rule, Direction),
                reject_fixture_(Base#{type => unknown_image}, Rule, Direction)
            end, Fields),
            reject_fixture_(#{type => Type}, Rule, Direction)
        end, [from_json, to_json])
    end, [
        {<<"UserInput">>, image, [url, fileId]}
      , {<<"ContentItem">>, input_image, [image_url, file_id]}
      , {<<"FunctionCallOutputContentItem">>, input_image, [image_url, file_id]}
    ]),
    McpPath = filename:join(Root, <<"McpServerElicitationRequestParams.json">>),
    {ok, McpSchemaJson} = file:read_file(McpPath),
    McpRules = klsn_rule_generator:from_json_schema(jsone:decode(McpSchemaJson)),
    lists:foreach(fun(Direction) ->
        Rule = maps:get(Direction, McpRules),
        Common = #{
            serverName => <<"server">>
          , threadId => <<"thread">>
          , turnId => <<"turn">>
          , message => <<"message">>
        },
        Inputs = [
            Common#{mode => url, url => <<"https://example.com">>, elicitationId => <<"id">>}
          , Common#{mode => form, requestedSchema => #{type => object, properties => #{}}}
          , Common#{mode => 'openai/form', requestedSchema => #{}}
          , Common#{mode => openaiForm, requestedSchema => #{}}
        ],
        lists:foreach(fun(Input) ->
            Input = normalize_fixture_(Input, Rule, Direction),
            lists:foreach(fun(Key) ->
                reject_fixture_(maps:remove(Key, Input), Rule, Direction)
            end, [serverName, threadId, message, mode]),
            NoTurn = maps:remove(turnId, Input),
            NoTurn = normalize_fixture_(NoTurn, Rule, Direction),
            NullTurn = Input#{turnId => null},
            NullTurn = normalize_fixture_(NullTurn, Rule, Direction)
        end, Inputs)
    end, [from_json, to_json]).

-spec codex_167_nested_choices(ct_suite:ct_config()) -> ok.
codex_167_nested_choices(Config) ->
    Path = filename:join([
        ?config(data_dir, Config)
      , <<"codex-app-server-schema-0.167.0">>
      , <<"ImageGenerationThreadItem.json">>
    ]),
    {ok, SchemaJson} = file:read_file(Path),
    Rules = klsn_rule_generator:from_json_schema(jsone:decode(SchemaJson)),
    Common = #{
        id => <<"item">>
      , status => <<"completed">>
      , type => imageGeneration
    },
    lists:foreach(fun(Direction) ->
        Rule = maps:get(Direction, Rules),
        lists:foreach(fun(Field) ->
            Base = Common#{Field => <<"image">>},
            Inputs = [
                Base
              , Base#{startedAtMs => 123, completedAtMs => 456}
              , Base#{startedAtMs => null, completedAtMs => null}
              , Base#{
                    startedAtMs => 123
                  , completedAtMs => 456
                  , generationId => <<"generation">>
                  , revisedPrompt => <<"prompt">>
                  , savedPath => <<"/image.png">>
                  , transparentBackground => true
                  , inputImages => [#{
                        path => <<"/input.png">>
                      , sha256 => <<"hash">>
                    }]
                  , failure => #{
                        type => usageLimitExceeded
                      , limitId => <<"limit">>
                      , resetsAt => null
                    }
                }
            ],
            lists:foreach(fun(Input) ->
                Input = normalize_fixture_(Input, Rule, Direction)
            end, Inputs),
            lists:foreach(fun(Key) ->
                reject_fixture_(maps:remove(Key, Base), Rule, Direction)
            end, [id, status, type, Field]),
            reject_fixture_(Base#{type => unknown_image}, Rule, Direction)
        end, [fileId, result]),
        %% Upstream puts fileId first so migration items keep that reference.
        Both = Common#{
            fileId => <<"file">>
          , result => <<>>
          , startedAtMs => 123
          , completedAtMs => 456
        },
        Expected = maps:remove(result, Both),
        Expected = klsn_rule:normalize(fixture_input_(Both, Direction), Rule),
        reject_fixture_(Common, Rule, Direction)
    end, [from_json, to_json]).

-spec fixture_input_(klsn_rule:input(), from_json | to_json) -> klsn_rule:input().
fixture_input_(Input, from_json) ->
    jsone:decode(jsone:encode(Input));
fixture_input_(Input, to_json) ->
    Input.

-spec normalize_fixture_(klsn_rule:input(), klsn_rule:rule(), from_json | to_json) -> klsn_rule:output().
normalize_fixture_(Input, Rule, Direction) ->
    Output = klsn_rule:normalize(fixture_input_(Input, Direction), Rule),
    %% Encoding must retain the same JSON field names and enum spellings.
    Json = jsone:decode(jsone:encode(Input)),
    Json = jsone:decode(jsone:encode(Output)),
    Output.

-spec reject_fixture_(klsn_rule:input(), klsn_rule:rule(), from_json | to_json) ->
    {reject, klsn_rule:reason()}.
reject_fixture_(Input, Rule, Direction) ->
    {reject, _} = klsn_rule:eval(fixture_input_(Input, Direction), Rule, #{}).
