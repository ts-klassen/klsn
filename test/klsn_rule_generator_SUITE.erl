-module(klsn_rule_generator_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0,
         schemas_match_expected/1]).

all() ->
    [schemas_match_expected].

schemas_match_expected(Config) ->
    DataDir = ?config(data_dir, Config),
    SchemaRoot = filename:join(DataDir, "codex-app-server-schema-0.98.0"),
    SchemaPaths = schema_paths_(SchemaRoot),
    ExpectedPairs = expected_pairs_(SchemaRoot, SchemaPaths),
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

relpath_(Base, Path) ->
    BaseParts = filename:split(filename:absname(Base)),
    PathParts = filename:split(filename:absname(Path)),
    case lists:prefix(BaseParts, PathParts) of
        true ->
            filename:join(lists:nthtail(length(BaseParts), PathParts));
        false ->
            Path
    end.
