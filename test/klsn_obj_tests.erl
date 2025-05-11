-module(klsn_obj_tests).
-include_lib("eunit/include/eunit.hrl").

%% Tests for get/2
get_2_test() ->
    Key1 = {tuple1, tuple2, [1,2,3, {{4,5,6}, #{7=>#{8=>9}}, [a,b,c]}]},
    Key2 = #{ key3 => [], key4 => {} },
    Obj = #{
        key1 => Key1
      , key2 => Key2
    },
    ?assertEqual(Obj, klsn_obj:get([], Obj)),
    
    ?assertEqual(Key1, klsn_obj:get([{m,key1}], Obj)),
    ?assertEqual(Key2, klsn_obj:get([{m,key2}], Obj)),
    ?assertError(not_found, klsn_obj:get([{m,key3}], Obj)),

    ?assertEqual(tuple1, klsn_obj:get([{m,key1},{t,1}], Obj)),
    ?assertEqual(tuple2, klsn_obj:get([{m,key1},{t,2}], Obj)),
    ?assertEqual(1, klsn_obj:get([{m,key1},{t,3},{l,1}], Obj)),
    ?assertEqual(2, klsn_obj:get([{m,key1},{t,3},{l,2}], Obj)),
    ?assertEqual(3, klsn_obj:get([{m,key1},{t,3},{l,3}], Obj)),
    ?assertEqual(4, klsn_obj:get([{m,key1},{t,3},{l,4},{t,1},{t,1}], Obj)),
    ?assertEqual(5, klsn_obj:get([{m,key1},{t,3},{l,4},{t,1},{t,2}], Obj)),
    ?assertEqual(6, klsn_obj:get([{m,key1},{t,3},{l,4},{t,1},{t,3}], Obj)),
    ?assertEqual(9, klsn_obj:get([{m,key1},{t,3},{l,4},{t,2},{m,7},{m,8}], Obj)),
    ?assertEqual(a, klsn_obj:get([{m,key1},{t,3},{l,4},{t,3},{l,1}], Obj)),
    ?assertEqual(b, klsn_obj:get([{m,key1},{t,3},{l,4},{t,3},{l,2}], Obj)),
    ?assertEqual(c, klsn_obj:get([{m,key1},{t,3},{l,4},{t,3},{l,3}], Obj)),

    ?assertEqual([], klsn_obj:get([{m,key2}, {m,key3}], Obj)),
    ?assertError(not_found, klsn_obj:get([{m,key2}, {m,key3}, {l,1}], Obj)),
    ?assertEqual({}, klsn_obj:get([{m,key2}, {m,key4}], Obj)),
    ?assertError(not_found, klsn_obj:get([{m,key2}, {m,key4}, {t,1}], Obj)),

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

    ok.

%% Tests for lookup/2
lookup_2_test() ->
    ?assertEqual({value, elem}, klsn_obj:lookup([1], {elem})),
    ?assertEqual(none, klsn_obj:lookup([2], {elem})),
    ?assertEqual(none, klsn_obj:lookup([1, 2, 3], {elem})),
    ok.

%% Tests for find/2
find_2_test() ->
    Obj = #{
        key1 => {tuple1, tuple2, [1,2,3, {{4,5,6}, #{7=>#{8=>9}}, [a,b,c]}]}
      , key2 => #{ key3 => [], key4 => {} }
    },
    AllPath = [
        []
      , [{m,key1}]
      , [{m,key1},{t,1}]
      , [{m,key1},{t,2}]
      , [{m,key1},{t,3}]
      , [{m,key1},{t,3},{l,1}]
      , [{m,key1},{t,3},{l,2}]
      , [{m,key1},{t,3},{l,3}]
      , [{m,key1},{t,3},{l,4}]
      , [{m,key1},{t,3},{l,4},{t,1}]
      , [{m,key1},{t,3},{l,4},{t,1},{t,1}]
      , [{m,key1},{t,3},{l,4},{t,1},{t,2}]
      , [{m,key1},{t,3},{l,4},{t,1},{t,3}]
      , [{m,key1},{t,3},{l,4},{t,2}]
      , [{m,key1},{t,3},{l,4},{t,2},{m,7}]
      , [{m,key1},{t,3},{l,4},{t,2},{m,7},{m,8}]
      , [{m,key1},{t,3},{l,4},{t,3}]
      , [{m,key1},{t,3},{l,4},{t,3},{l,1}]
      , [{m,key1},{t,3},{l,4},{t,3},{l,2}]
      , [{m,key1},{t,3},{l,4},{t,3},{l,3}]
      , [{m,key2}]
      , [{m,key2},{m,key3}]
      , [{m,key2},{m,key4}]
    ],
    ?assertEqual(AllPath, klsn_obj:find(fun(_)->true end, Obj)),
    ?assertEqual([], klsn_obj:find(fun(_)->false end, Obj)),
    ?assertEqual(
        [
            [{m,key1},{t,3},{l,1}]
          , [{m,key1},{t,3},{l,3}]
          , [{m,key1},{t,3},{l,4},{t,1},{t,2}]
          , [{m,key1},{t,3},{l,4},{t,2},{m,7},{m,8}]
        ]
      , klsn_obj:find(
            fun
                (N) when N rem 2 =:= 1->
                    true;
                (_) ->
                    false
            end
          , Obj
        )
    ),
    ?assertEqual(
        [
            []
          , [{m,key1},{t,3},{l,4},{t,2}]
          , [{m,key1},{t,3},{l,4},{t,2},{m,7}]
          , [{m,key2}]
        ]
      , klsn_obj:find(
            fun
                (M) when is_map(M) ->
                    true;
                (_) ->
                    false
            end
          , Obj
        )
    ),
    ?assertEqual(
        [
            [{m,key1},{t,3},{l,4},{t,2}]
          , [{m,key1},{t,3},{l,4},{t,2},{m,7}]
        ]
      , klsn_obj:find(
            fun
                (M, [{m,key1}|_]) when is_map(M) ->
                    true;
                (_, _) ->
                    false
            end
          , Obj
        )
    ),
    ok.

