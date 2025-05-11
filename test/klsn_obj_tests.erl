-module(klsn_obj_tests).
-include_lib("eunit/include/eunit.hrl").

%% Tests for get1
get_1_test() ->
    Key1 = {tuple1, tuple2, [1,2,3, {{4,5,6}, #{7=>#{8=>9}}, [a,b,c]}]},
    Key2 = #{ key3 => [], key4 => {} },
    Obj = #{
        key1 => Key1
      , key2 => Key2
    },
    ?assertEqual(Obj, klsn_obj:get([], Obj)),
    
    ?assertEqual(Key1, klsn_obj:get([key1], Obj)),
    ?assertEqual(Key2, klsn_obj:get([key2], Obj)),

    Map = #{a => 1, b => 2},
    ?assertEqual(1, klsn_obj:get([{map, a}], Map)),
    ?assertEqual(2, klsn_obj:get([{m, b}], Map)),
    ?assertEqual(1, klsn_obj:get([{raw, a}], Map)),
    ?assertEqual(2, klsn_obj:get([b], Map)),
    ?assertError(not_found, klsn_obj:get([{list, 1}], Map)),
    ?assertError(not_found, klsn_obj:get([{l, 1}], Map)),
    ?assertError(not_found, klsn_obj:get([{tuple, 1}], Map)),
    ?assertError(not_found, klsn_obj:get([{t, 1}], Map)),

    List = [10, 20, 30],
    ?assertEqual(10, klsn_obj:get([{list, 1}], List)),
    ?assertEqual(20, klsn_obj:get([{l, 2}], List)),
    ?assertEqual(30, klsn_obj:get([{raw, 3}], List)),
    ?assertEqual(20, klsn_obj:get([2], List)),
    ?assertError(not_found, klsn_obj:get([{map, a}], List)),
    ?assertError(not_found, klsn_obj:get([{m, a}], List)),
    ?assertError(not_found, klsn_obj:get([{list, 4}], List)),
    ?assertError(not_found, klsn_obj:get([{t, 1}], List)),
    ?assertError(not_found, klsn_obj:get([{tuple, 1}], List)),
    ?assertError(not_found, klsn_obj:get([{raw, 0}], List)),

    Tuple = {alpha, beta, gamma},
    ?assertEqual(alpha, klsn_obj:get([{tuple, 1}], Tuple)),
    ?assertEqual(beta, klsn_obj:get([{t, 2}], Tuple)),
    ?assertEqual(gamma, klsn_obj:get([{raw, 3}], Tuple)),
    ?assertEqual(beta, klsn_obj:get([2], Tuple)),
    ?assertError(not_found, klsn_obj:get([{list, 1}], Tuple)),
    ?assertError(not_found, klsn_obj:get([{l, 1}], Tuple)),
    ?assertError(not_found, klsn_obj:get([{map, key}], Tuple)),
    ?assertError(not_found, klsn_obj:get([{m, key}], Tuple)),
    ?assertError(not_found, klsn_obj:get([{tuple, 4}], Tuple)),
    ?assertError(not_found, klsn_obj:get([{raw, 0}], Tuple)),

    ?assertError(not_found, klsn_obj:get([key3], Obj)),

    ok.

%% Tests for lookup/1
lookup_1_test() ->
    ?assertEqual({value, elem}, klsn_obj:lookup([1], {elem})),
    ?assertEqual(none, klsn_obj:lookup([2], {elem})),
    ok.

