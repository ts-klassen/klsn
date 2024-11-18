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
    ok.

