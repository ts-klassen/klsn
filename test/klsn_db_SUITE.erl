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
    true = klsn_db:exists(DB, Key),
    false = klsn_db:exists(DB, <<"non-existing-key">>),
    false = klsn_db:exists(DB, <<>>),
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
    true = klsn_db:exists(DB, {raw, <<"_design/index">>}),
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

    %% Bulk tests
    [
        #{<<"_id">> := <<"bulk1">>, <<"_rev">> := _, <<"bulk">> := 0}
      , #{<<"_id">> := <<"bulk3">>, <<"_rev">> := _, <<"bulk">> := 0}
    ] = klsn_db:bulk_upsert(DB, [<<"bulk1">>, <<"bulk3">>], fun(none) ->
        #{bulk => 0}
    end),
    [
        none
      , {value, #{<<"_id">> := <<"bulk1">>, <<"_rev">> := BulkRev1, <<"bulk">> := 0}}
      , none
      , {value, #{<<"_id">> := <<"bulk3">>, <<"_rev">> := BulkRev3, <<"bulk">> := 0}}
      , none
    ] = klsn_db:bulk_lookup(DB, [<<"bulk0">>, <<"bulk1">>, <<"bulk2">>, <<"bulk3">>, <<"bulk4">>]),
    [
        #{<<"_id">> := <<"bulk0">>, <<"_rev">> := _, <<"bulk">> := 0}
      , #{<<"_id">> := <<"bulk1">>, <<"_rev">> := _, <<"bulk">> := 1}
      , #{<<"_id">> := <<"bulk2">>, <<"_rev">> := _, <<"bulk">> := 0}
      , #{<<"_id">> := <<"bulk3">>, <<"_rev">> := _, <<"bulk">> := 3}
      , #{<<"_id">> := <<"bulk4">>, <<"_rev">> := _, <<"bulk">> := 0}
    ] = klsn_db:bulk_upsert(DB, [<<"bulk0">>, <<"bulk1">>, <<"bulk2">>, <<"bulk3">>, <<"bulk4">>], fun
        (none) ->
            #{bulk => 0};
        ({value, Doc=#{<<"_id">> := <<"bulk1">>, <<"_rev">> := Rev}}) when Rev =:= BulkRev1 ->
            Doc#{<<"bulk">> := 1};
        ({value, Doc=#{<<"_id">> := <<"bulk3">>, <<"_rev">> := Rev}}) when Rev =:= BulkRev3 ->
            Doc#{<<"bulk">> := 3}
    end),
    [
        {value, #{<<"_id">> := <<"bulk0">>, <<"_rev">> := _, <<"bulk">> := 0}}
      , {value, #{<<"_id">> := <<"bulk1">>, <<"_rev">> := _, <<"bulk">> := 1}}
      , {value, #{<<"_id">> := <<"bulk2">>, <<"_rev">> := _, <<"bulk">> := 0}}
      , {value, #{<<"_id">> := <<"bulk3">>, <<"_rev">> := _, <<"bulk">> := 3}}
      , {value, #{<<"_id">> := <<"bulk4">>, <<"_rev">> := _, <<"bulk">> := 0}}
    ] = lists:map(fun(Id) ->
        klsn_db:lookup(DB, Id)
    end, [<<"bulk0">>, <<"bulk1">>, <<"bulk2">>, <<"bulk3">>, <<"bulk4">>]),

    %% Mango find tests (CouchDB _find)
    {_, _} = klsn_db:create_doc(DB, #{<<"mango">> => true, <<"n">> => 1}),
    {_, _} = klsn_db:create_doc(DB, #{<<"mango">> => true, <<"n">> => 2}),
    {_, _} = klsn_db:create_doc(DB, #{<<"mango">> => true, <<"n">> => 3}),
    Docs = klsn_db:mango_find(DB, #{
        <<"selector">> => #{
            <<"mango">> => true,
            <<"n">> => #{<<"$gte">> => 2}
        }
    }),
    2 = length([D || D <- Docs, maps:get(<<"mango">>, D, false) =:= true]),
    true = lists:any(fun(#{<<"n">> := 2}) -> true; (_) -> false end, Docs),
    true = lists:any(fun(#{<<"n">> := 3}) -> true; (_) -> false end, Docs),
    %% Mango index tests (CouchDB /_index)
    IndexRes = klsn_db:mango_index(DB, #{
        <<"index">> => #{<<"fields">> => [<<"mango">>, <<"n">>]},
        <<"name">> => <<"mango_n_idx">>
    }),
    true = lists:member(maps:get(<<"result">>, IndexRes), [<<"created">>, <<"exists">>]),
    _ = maps:get(<<"name">>, IndexRes),
    %% id may or may not be present depending on server, skip strict check
    %% Mango explain tests (CouchDB /_explain)
    ExplainRes = klsn_db:mango_explain(DB, #{
        <<"selector">> => #{
            <<"mango">> => true
        },
        <<"use_index">> => <<"mango_n_idx">>
    }),
    true = is_map(ExplainRes),
    ok = try klsn_db:mango_find(non_existing, #{<<"selector">> => #{}}) catch
        error:not_found -> ok
    end,
    ok = try klsn_db:mango_index(non_existing, #{
        <<"index">> => #{<<"fields">> => [<<"mango">>]}
    }) catch
        error:not_found -> ok
    end,
    ok = try klsn_db:mango_explain(non_existing, #{<<"selector">> => #{}}) catch
        error:not_found -> ok
    end,
    ok.
