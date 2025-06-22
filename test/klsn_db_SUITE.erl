-module(klsn_db_SUITE).
-include_lib("common_test/include/ct.hrl").

%% Export the suite callback and test cases
-export([all/0,
         main/1]).

%% Define the test suite with all/0
all() ->
    [
        main
    ].

%% Just make sure there are no errors

main(_Config) ->
    DB = binary_to_atom(iolist_to_binary([
        <<"test_klsn_db_suite_">>,
        re:replace(klsn_db:time_now(), <<"[^0-9]">>, <<"_">>, [global])
    ])),
    ok = klsn_db:create_db(DB),
    ok = try klsn_db:create_db(DB) catch
        error:exists ->
            ok
    end,
    {Key, _} = klsn_db:create_doc(DB, #{<<"test_val">> => <<"test123">>}),
    ok = try
        klsn_db:create_doc(non_existing, #{})
    catch
        error:not_found ->
            ok
    end,
    #{<<"test_val">> := <<"test123">>} = klsn_db:get(DB, Key),
    ok = try klsn_db:get(DB, <<"non-existing-key">>) catch
        error:not_found ->
            ok
    end,
    #{<<"test_val">> := <<"test456">>} = klsn_db:update(DB, Key, fun
        (Doc) ->
            Doc#{<<"test_val">> := <<"test456">>}
    end),
    {value, #{<<"test_val">> := <<"test456">>}} = klsn_db:lookup(DB, Key),
    ok = try klsn_db:update(DB, <<"not-any-key">>, fun(Doc) -> Doc end) catch
        error:not_found ->
            ok
    end,
    #{<<"test_val">> := <<"test789">>} = klsn_db:upsert(DB, klsn_db:new_id(),
        fun(none) ->
            #{<<"test_val">> => <<"test789">>}
    end),
    #{} = klsn_db:upsert(DB, {raw, <<"_design/index">>}, fun(none) ->
        #{views => #{test => #{
            map => <<"function (doc) {emit(doc._id, 1);}">>
        }}, language => javascript}
    end),
    #{} = klsn_db:upsert(DB, {raw, <<"_design/index">>}, fun({value, Doc}) ->
        Doc#{views => #{test => #{
            map => <<"function (doc) {emit(doc._id, 2);}">>
        }}, language => javascript}
    end),
    {value, #{}} = klsn_db:lookup(DB, {raw, <<"_design/index">>}),
    ok = try
        klsn_db:update(DB, <<"non_existing">>, fun(_) ->
            error(unreachable)
        end)
    catch
        error:not_found ->
            ok
    end,
    none = klsn_db:lookup(DB, {raw, <<"_design/none">>}),
    none = klsn_db:lookup(DB, <<>>),
    #{} = klsn_db:upsert(DB, <<>>, fun(none)-> #{} end),
    ConflictRef = make_ref(),
    Self = self(),
    ConflictPid = spawn_link(fun() ->
        receive ConflictRef ->
            ok
        end,
        klsn_db:upsert(DB, <<"conflict-once">>, fun(none) ->
            #{}
        end),
        Self ! ConflictRef
    end),
    klsn_db:upsert(DB, <<"conflict-once">>, fun
        (none) ->
            ConflictPid ! ConflictRef,
            receive ConflictRef ->
                ok
            end,
            #{};
        ({value, Doc}) ->
            Doc
    end),
    false = erlang:is_process_alive(ConflictPid),
    %% Ensure upsert/3 returns error:not_found when the target DB is missing
    ok = try
        klsn_db:upsert(non_existing, klsn_db:new_id(), fun(none) ->
            #{}
        end)
    catch
        error:not_found ->
            ok
    end,
    ok.

