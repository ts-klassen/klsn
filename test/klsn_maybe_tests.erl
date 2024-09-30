-module(klsn_maybe_tests).
-include_lib("eunit/include/eunit.hrl").

%% Tests for get_value/1
get_value_1_test() ->
    %% Test with a valid value
    ?assertEqual(42, klsn_maybe:get_value({value, 42})),
    
    %% Test with 'none' to ensure it raises a badarg error
    ?assertMatch(
        {error, badarg, [none]},
        catch klsn_maybe:get_value(none)
    ).

%% Tests for get_value/2
get_value_2_test() ->
    %% Test with a valid value and default
    ?assertEqual(42, klsn_maybe:get_value({value, 42}, "default")),
    
    %% Test with 'none' and a default value
    ?assertEqual("default", klsn_maybe:get_value(none, "default")),
    
    %% Test with invalid arguments to ensure it raises a badarg error
    ?assertMatch(
        {error, badarg, [{invalid, args}]},
        catch klsn_maybe:get_value({invalid, args}, "default")
    ).
