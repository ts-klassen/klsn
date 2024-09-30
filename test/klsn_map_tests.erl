-module(klsn_map_tests).
-include_lib("eunit/include/eunit.hrl").

%% Tests for lookup/2
lookup_test() ->
    %% Test with an empty key
    Map = #{a => 1, b => 2},
    ?assertEqual({value, Map}, klsn_map:lookup([], Map)),

    %% Test with a valid single-level key
    ?assertEqual({value, 1}, klsn_map:lookup([a], Map)),

    %% Test with a nested key
    NestedMap = #{c => #{d => 4}},
    ComplexMap = #{a => 1, b => 2, c => NestedMap},
    ?assertEqual({value, 4}, klsn_map:lookup([c, d], ComplexMap)),

    %% Test with a non-existing key
    ?assertEqual(none, klsn_map:lookup([e], Map)),

    %% Test with a nested non-existing key
    ?assertEqual(none, klsn_map:lookup([c, e], ComplexMap)).

%% Tests for get/2
get_2_test() ->
    Map = #{a => 1, b => 2},

    %% Test retrieving an existing key
    ?assertEqual(1, klsn_map:get([a], Map)),

    %% Test retrieving a nested existing key
    NestedMap = #{c => #{d => 4}},
    ComplexMap = #{a => 1, b => 2, c => NestedMap},
    ?assertEqual(4, klsn_map:get([c, d], ComplexMap)),

    %% Test retrieving a non-existing key should raise a not_found error
    ?assertError({not_found, [[e], Map]}, klsn_map:get([e], Map)),

    %% Test retrieving a non-existing nested key should raise a not_found error
    ?assertError({not_found, [[c, e], ComplexMap]}, klsn_map:get([c, e], ComplexMap)).

%% Tests for get/3
get_3_test() ->
    Map = #{a => 1, b => 2},

    %% Test retrieving an existing key
    ?assertEqual(1, klsn_map:get([a], Map, 0)),

    %% Test retrieving a nested existing key
    NestedMap = #{c => #{d => 4}},
    ComplexMap = #{a => 1, b => 2, c => NestedMap},
    ?assertEqual(4, klsn_map:get([c, d], ComplexMap, 0)),

    %% Test retrieving a non-existing key returns the default value
    ?assertEqual(0, klsn_map:get([e], Map, 0)),

    %% Test retrieving a non-existing nested key returns the default value
    ?assertEqual(0, klsn_map:get([c, e], ComplexMap, 0)).

%% Tests for upsert/3
upsert_test() ->
    %% Test upserting a new top-level key
    Map1 = #{a => 1, b => 2},
    UpdatedMap1 = klsn_map:upsert([c], 3, Map1),
    ?assertEqual(3, maps:get(c, UpdatedMap1)),

    %% Test upserting an existing top-level key
    UpdatedMap2 = klsn_map:upsert([a], 10, UpdatedMap1),
    ?assertEqual(10, maps:get(a, UpdatedMap2)),

    %% Test upserting a nested key
    NestedMap = #{c => #{d => 4}},
    Map2 = #{a => 1, b => 2, c => NestedMap},
    UpdatedMap3 = klsn_map:upsert([c, e], 5, Map2),
    ?assertEqual(5, maps:get(e, maps:get(c, UpdatedMap3))),

    %% Test upserting to a nested key that doesn't exist yet
    UpdatedMap4 = klsn_map:upsert([c, f], 6, Map2),
    ?assertEqual(6, maps:get(f, maps:get(c, UpdatedMap4))),

    %% Test upserting with an empty key should replace the entire map
    UpdatedMap5 = klsn_map:upsert([], #{x => 100}, UpdatedMap4),
    ?assertEqual(#{x => 100}, UpdatedMap5).
