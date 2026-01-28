-module(klsn_maybe_tests).
-include_lib("eunit/include/eunit.hrl").

%% Tests for get_value/1
get_value_1_test() ->
    %% Test with a valid value
    ?assertEqual(42, klsn_maybe:get_value({value, 42})),
    
    %% Test with 'none' to ensure it raises a badarg error
    ?assertError(badarg, klsn_maybe:get_value(none)).

%% Tests for has_value/1
has_value_1_test() ->
    ?assertEqual(true, klsn_maybe:has_value({value, 42})),
    ?assertEqual(false, klsn_maybe:has_value(none)),
    ?assertError(badarg, klsn_maybe:has_value({invalid, args})).

%% Tests for get_value/2
get_value_2_test() ->
    %% Test with a valid value and default
    ?assertEqual(42, klsn_maybe:get_value({value, 42}, "default")),
    
    %% Test with 'none' and a default value
    ?assertEqual("default", klsn_maybe:get_value(none, "default")),
    
    %% Test with invalid arguments to ensure it raises a badarg error
    ?assertError(badarg, klsn_maybe:get_value({invalid, args}, "default")).

filtermap_2_lists_test() ->
    ?assertEqual(
        [2, 4, 6, 8],
        klsn_maybe:filtermap(fun
            (X) when X rem 2 =:= 0 ->
                {value, X};
            (_) ->
                none
        end, [1, 2, 3, 4, 5, 6, 7, 8, 9])
    ).

filtermap_2_maps_test() ->
    ?assertEqual(
        #{b => 2, d => 4, f => 6, h => 8},
        klsn_maybe:filtermap(fun
            (_Key, X) when X rem 2 =:= 0 ->
                {value, X};
            (_, _) ->
                none
        end, #{a=>1, b=>2, c=>3, d=>4, e=>5, f=>6, g=>7, h=>8, i=>9})
    ).
