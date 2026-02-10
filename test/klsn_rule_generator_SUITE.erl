-module(klsn_rule_generator_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0,
         schemas_match_expected/1]).

all() ->
    [schemas_match_expected].

schemas_match_expected(Config) ->
    DataDir = ?config(data_dir, Config),
    SchemaRoot = filename:join(DataDir, "codex-app-server-schema-0.98.0"),
    ExpectedRelPaths = expected_relpaths_(DataDir),
    ExpectedPairs = expected_pairs_(SchemaRoot, ExpectedRelPaths),
    report_missing_expected_(SchemaRoot, ExpectedPairs),
    lists:foreach(fun({ExpectedPath, SchemaPath}) ->
        {ok, [ExpectedRule]} = file:consult(ExpectedPath),
        {ok, Bin} = file:read_file(SchemaPath),
        Schema = jsone:decode(Bin),
        Actual = klsn_rule_generator:from_json_schema(Schema),
        Expected = expected_rules_(ExpectedRule),
        case Actual =:= Expected of
            true ->
                ok;
            false ->
                RelPath = relpath_(SchemaRoot, SchemaPath),
                ct:fail({schema_mismatch, RelPath, {expected, Expected}, {actual, Actual}})
        end
    end, ExpectedPairs),
    ok.

expected_rules_(#{from_json := _, to_json := _} = Rules) ->
    Rules;
expected_rules_(Rule) ->
    #{from_json => Rule, to_json => Rule}.

expected_relpaths_(DataDir) ->
    ManifestPath = filename:join(DataDir, "expected_terms.list"),
    {ok, Bin} = file:read_file(ManifestPath),
    Lines = string:split(binary_to_list(Bin), "\n", all),
    lists:filtermap(fun(Line) ->
        Trimmed = string:trim(Line),
        case Trimmed of
            "" -> false;
            [$# | _] -> false;
            _ -> {true, Trimmed}
        end
    end, Lines).

expected_pairs_(SchemaRoot, RelPaths) ->
    lists:map(fun(RelPath) ->
        ExpectedPath = filename:join(SchemaRoot, RelPath),
        case filelib:is_file(ExpectedPath) of
            true ->
                SchemaPath = filename:rootname(ExpectedPath, ".term") ++ ".json",
                case filelib:is_file(SchemaPath) of
                    true ->
                        {ExpectedPath, SchemaPath};
                    false ->
                        ct:fail({missing_schema_file, RelPath})
                end;
            false ->
                ct:fail({missing_expected_file, RelPath})
        end
    end, RelPaths).

report_missing_expected_(SchemaRoot, ExpectedPairs) ->
    JsonPaths = filelib:fold_files(
        SchemaRoot,
        ".*\\.json$",
        true,
        fun(Path, Acc) -> [Path | Acc] end,
        []
    ),
    ExpectedJsonPaths = [SchemaPath || {_ExpectedPath, SchemaPath} <- ExpectedPairs],
    Missing = ordsets:subtract(
        ordsets:from_list(JsonPaths),
        ordsets:from_list(ExpectedJsonPaths)
    ),
    case Missing of
        [] ->
            ok;
        _ ->
            Sample = lists:sublist(lists:sort(Missing), 5),
            SampleRel = [relpath_(SchemaRoot, Path) || Path <- Sample],
            ct:comment("Missing expected rule files: ~p (sample: ~p)", [length(Missing), SampleRel])
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
