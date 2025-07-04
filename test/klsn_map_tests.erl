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
    ?assertEqual({value, 4}, klsn_map:lookup([c, c, d], ComplexMap)),

    %% Test with a non-existing key
    ?assertEqual(none, klsn_map:lookup([e], Map)),

    %% Test with a nested non-existing key
    ?assertEqual(none, klsn_map:lookup([c, e], ComplexMap)),

    %% Test lookup with a non-map value (should return none)
    ?assertEqual(none, klsn_map:lookup([a], not_a_map)),

    %% Test lookup with a nested key leading to a non-map value (should return none)
    ?assertEqual(none, klsn_map:lookup([a, b], #{a => not_a_map})).

%% Tests for get/2
get_2_test() ->
    Map = #{a => 1, b => 2},

    %% Test retrieving an existing key
    ?assertEqual(1, klsn_map:get([a], Map)),

    %% Test retrieving a nested existing key
    NestedMap = #{c => #{d => 4}},
    ComplexMap = #{a => 1, b => 2, c => NestedMap},
    ?assertEqual(4, klsn_map:get([c, c, d], ComplexMap)),

    %% Test retrieving a non-existing key should raise a not_found error
    ?assertError(not_found, klsn_map:get([e], Map)),

    %% Test retrieving a non-existing nested key should raise a not_found error
    ?assertError(not_found, klsn_map:get([c, e], ComplexMap)).

%% Tests for get/3
get_3_test() ->
    Map = #{a => 1, b => 2},

    %% Test retrieving an existing key
    ?assertEqual(1, klsn_map:get([a], Map, 0)),

    %% Test retrieving a nested existing key
    NestedMap = #{c => #{d => 4}},
    ComplexMap = #{a => 1, b => 2, c => NestedMap},
    ?assertEqual(4, klsn_map:get([c, c, d], ComplexMap, 0)),

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
    ?assertEqual(#{x => 100}, UpdatedMap5),

    %% Test upserting with a non-map as the base map (should handle gracefully)
    ?assertError({badmap, not_a_map}, klsn_map:upsert([a], 100, not_a_map)),

    %% Test upserting with a nested key leading to a non-map value (should handle gracefully)
    ?assertError({badmap, not_a_map}, klsn_map:upsert([a, b], 100, #{a => not_a_map})),

    %% Test upserting into a path where intermediate key is missing (triggers [none|Maps])
    OriginalMap = #{a => 1, b => 2},
    UpdatedMap6 = klsn_map:upsert([c, d], 100, OriginalMap),
    ?assertEqual(#{a => 1, b => 2, c => #{d => 100}}, UpdatedMap6).

%% Tests for filter/1
filter_test() ->
    ?assertEqual(#{}, klsn_map:filter(#{})),
    ?assertEqual(#{}, klsn_map:filter(#{ a => none })),
    ?assertEqual(#{}, klsn_map:filter(#{ a => none, b => none })),
    ?assertEqual(#{ b => 2}, klsn_map:filter(#{ a => none, b => {value, 2} })),
    ?assertEqual(#{ a => 1, b => 2}, klsn_map:filter(#{
        a => {value, 1}
      , b => {value, 2}
    })).

%% Tests for invert/1
invert_test() ->
    ?assertEqual(#{}, klsn_map:invert(#{})),
    ?assertEqual(#{ 1 => a, 2 => b}, klsn_map:invert(#{
        a => 1
      , b => 2
    })).

%% Tests for remove/2
remove_test() ->
    %% Remove a top-level key
    Map1 = #{a => 1, b => 2},
    ?assertEqual(#{b => 2}, klsn_map:remove([a], Map1)),

    %% Remove a nested key
    Map2 = #{a => #{x => 10, y => 20}, b => 3},
    ?assertEqual(#{a => #{y => 20}, b => 3}, klsn_map:remove([a, x], Map2)),

    %% Removing with an empty path clears the map
    ?assertEqual(#{}, klsn_map:remove([], Map2)),

    %% Removing a non-existing key/path leaves the map unchanged
    ?assertEqual(Map1, klsn_map:remove([c], Map1)),
    ?assertEqual(Map1, klsn_map:remove([a, z], Map1)),

    %% Path where an intermediate value is not a map – unchanged
    Map3 = #{a => 5},
    ?assertEqual(Map3, klsn_map:remove([a, x], Map3)).

