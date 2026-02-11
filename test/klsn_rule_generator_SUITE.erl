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
