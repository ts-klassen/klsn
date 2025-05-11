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

%% Tests for crud/3
crud_3_test() ->
    Key1 = {tuple1, tuple2, [1,2,3, {{4,5,6}, #{7=>#{8=>9}}, [a,b,c]}]},
    Key2 = #{ key3 => [], key4 => {} },
    Obj = #{
        key1 => Key1
      , key2 => Key2
    },

    ?assertEqual(
        Obj#{ key5 => value5 }
      , klsn_obj:crud(
            [{m,key5}]
          , fun
                (none) ->
                    {value, value5}
            end
          , Obj
        )
    ),
    ?assertEqual(
        #{ key2 => Key2 }
      , klsn_obj:crud(
            [{m,key1}]
          , fun
                ({value, _}) ->
                    none
            end
          , Obj
        )
    ),
    ?assertEqual(
        bad_return
      , try
            klsn_obj:crud(
                [{m,key1}]
              , fun
                    ({value, _}) ->
                        other
                end
              , Obj
            )
        of
            Res -> error({unexpected, Res})
        catch
            error:Error ->
                element(1, Error)
        end
    ),
    ?assertEqual(
        #{
            key1 => {
                tuple1
               , tuple2
               , [0,2,3, {{4,5,6}, #{7=>#{8=>9}}, [a,b,c]}]
             }
          , key2 => Key2
        }
      , klsn_obj:crud(
            [key1, 3, 1]
          , fun
                ({value, 1}) ->
                    {value, 0}
            end
          , Obj
        )
    ),
    ?assertEqual(
        #{
            key1 => {
                tuple1
               , tuple2
               , [1,3, {{4,5,6}, #{7=>#{8=>9}}, [a,b,c]}]
             }
          , key2 => Key2
        }
      , klsn_obj:crud(
            [{map,key1}, {tuple,3}, {list,2}]
          , fun
                ({value, 2}) ->
                    none
            end
          , Obj
        )
    ),
    ?assertEqual(
        #{
            key1 => {
                tuple1
               , tuple2
               , [1,2,3, {{4,5}, #{7=>#{8=>9}}, [a,b,c]}]
             }
          , key2 => Key2
        }
      , klsn_obj:crud(
            [{raw,key1}, {raw,3}, {raw,4}, {t,1},{t,3}]
          , fun
                ({value, 6}) ->
                    none
            end
          , Obj
        )
    ),
    ?assertEqual(
        #{
            key1 => [nil, nil, f]
          , key2 => Key2
        }
      , klsn_obj:crud(
            [{m,key1}, {l,3}]
          , fun
                (none) ->
                    {value, f}
            end
          , Obj
        )
    ),
    ?assertEqual(
        #{
            key1 => Key1
          , key2 => #{ key3 => [first], key4 => {} }
        }
      , klsn_obj:crud(
            [{m,key2}, {m,key3}, {l,1}]
          , fun
                (none) ->
                    {value, first}
            end
          , Obj
        )
    ),
    ?assertEqual(
        Obj
      , klsn_obj:crud(
            [{m,key2}, {m,key4}, {t,1}]
          , fun
                (none) ->
                    none
            end
          , Obj
        )
    ),
    ?assertEqual(
        nil
      , klsn_obj:crud(
            []
          , fun
                ({value, Obj0}) when Obj0 =:= Obj ->
                    none
            end
          , Obj
        )
    ),
    ?assertEqual(
        atom2
      , klsn_obj:crud(
            []
          , fun
                ({value, atom1}) ->
                    {value, atom2}
            end
          , atom1
        )
    ),
    ?assertEqual(
        #{ field1 => atom2 }
      , klsn_obj:crud(
            [field1]
          , fun
                (none) ->
                    {value, atom2}
            end
          , atom1
        )
    ),
    ?assertEqual(
        [[nil, {nil, nil, {nil, nil, nil, #{f1=>#{f2=>#{f3=>atom2}}}}}]]
      , klsn_obj:crud(
            [{list,1}, {l,2}, {tuple,3}, {t,4}, {raw,f1}, {map,f2}, {m,f3}]
          , fun
                (none) ->
                    {value, atom2}
            end
          , #{}
        )
    ),

    % Just for the sake of coverage.
    ?assertEqual(
        nil
      , klsn_obj:crud([field1], fun(none) -> none end, [])
    ),
    ?assertEqual(
        nil
      , klsn_obj:crud([field1], fun(none) -> none end, {})
    ),
    ?assertEqual(
        nil
      , klsn_obj:crud([{l,1}], fun(none) -> none end, #{})
    ),
    ?assertEqual(
        nil
      , klsn_obj:crud([{tuple,1}], fun(none) -> none end, #{})
    ),
    ?assertEqual(
        nil
      , klsn_obj:crud([{t,1}], fun(none) -> none end, #{})
    ),
    ?assertEqual(
        nil
      , klsn_obj:crud([{map,1}], fun(none) -> none end, [])
    ),
    ?assertEqual(
        nil
      , klsn_obj:crud([{m,1}], fun(none) -> none end, [])
    ),
    ?assertEqual(
        nil
      , klsn_obj:crud([{tuple,1}], fun(none) -> none end, [])
    ),
    ?assertEqual(
        nil
      , klsn_obj:crud([{t,1}], fun(none) -> none end, [])
    ),
    ?assertEqual(
        nil
      , klsn_obj:crud([{map,1}], fun(none) -> none end, {})
    ),
    ?assertEqual(
        nil
      , klsn_obj:crud([{m,1}], fun(none) -> none end, {})
    ),
    ?assertEqual(
        nil
      , klsn_obj:crud([{list,1}], fun(none) -> none end, {})
    ),

    ok.



