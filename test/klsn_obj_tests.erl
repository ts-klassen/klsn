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

any_rule_test() ->
    Rule = any,
    Valid = [],
    ?assertEqual(true, klsn_obj:validate(Rule, Valid)),
    ?assertEqual(Valid, klsn_obj:normalize(Rule, Valid)).

integer_rule_test() ->
    Rule = integer,
    Valid = 42,
    Invalid = 3.14,
    ?assertEqual(true, klsn_obj:validate(Rule, Valid)),
    ?assertEqual(false, klsn_obj:validate(Rule, Invalid)),
    ?assertEqual(Valid, klsn_obj:normalize(Rule, Valid)),
    ?assertEqual({invalid, Rule, Invalid}, invalid_reason(Rule, Invalid)).

to_integer_rule_test() ->
    Rule = to_integer,
    ?assertEqual(42, klsn_obj:normalize(Rule, <<"42">>)),
    ?assertEqual({invalid, Rule, <<"a">>}, invalid_reason(Rule, <<"a">>)),
    ?assertEqual(42, klsn_obj:normalize(Rule, "42")),
    ?assertEqual({invalid, Rule, "a"}, invalid_reason(Rule, "a")).

float_rule_test() ->
    Rule = float,
    Valid = 3.14,
    Invalid = 42,
    ?assertEqual(true, klsn_obj:validate(Rule, Valid)),
    ?assertEqual(false, klsn_obj:validate(Rule, Invalid)),
    ?assertEqual(Valid, klsn_obj:normalize(Rule, Valid)),
    ?assertEqual({invalid, Rule, Invalid}, invalid_reason(Rule, Invalid)).

to_float_rule_test() ->
    Rule = to_float,
    ?assertEqual(3.14, klsn_obj:normalize(Rule, <<"3.14">>)),
    ?assertEqual({invalid, Rule, <<"a">>}, invalid_reason(Rule, <<"a">>)),
    ?assertEqual(3.14, klsn_obj:normalize(Rule, "3.14")),
    ?assertEqual({invalid, Rule, "a"}, invalid_reason(Rule, "a")).

number_rule_test() ->
    Rule = number,
    Valid = 3.14,
    Invalid = <<"42">>,
    ?assertEqual(true, klsn_obj:validate(Rule, Valid)),
    ?assertEqual(false, klsn_obj:validate(Rule, Invalid)),
    ?assertEqual(Valid, klsn_obj:normalize(Rule, Valid)),
    ?assertEqual({invalid, Rule, Invalid}, invalid_reason(Rule, Invalid)).

to_number_rule_test() ->
    Rule = to_number,
    ?assertEqual(42, klsn_obj:normalize(Rule, <<"42">>)),
    ?assertEqual(3.14, klsn_obj:normalize(Rule, <<"3.14">>)),
    ?assertEqual({invalid, Rule, <<"a">>}, invalid_reason(Rule, <<"a">>)),
    ?assertEqual(42, klsn_obj:normalize(Rule, "42")),
    ?assertEqual(3.14, klsn_obj:normalize(Rule, "3.14")),
    ?assertEqual({invalid, Rule, "a"}, invalid_reason(Rule, "a")),
    ?assertEqual(42, klsn_obj:normalize(Rule, 42)),
    ?assertEqual(3.14, klsn_obj:normalize(Rule, 3.14)).

binstr_rule_test() ->
    Rule = binstr,
    Valid = <<"hello world">>,
    Invalid = "hello world",
    ?assertEqual(true, klsn_obj:validate(Rule, Valid)),
    ?assertEqual(false, klsn_obj:validate(Rule, Invalid)),
    ?assertEqual(Valid, klsn_obj:normalize(Rule, Valid)),
    ?assertEqual({invalid, Rule, Invalid}, invalid_reason(Rule, Invalid)).

to_binstr_rule_test() ->
    Rule = to_binstr,
    ?assertEqual(<<"hello">>, klsn_obj:normalize(Rule, <<"hello">>)),
    ?assertEqual(<<"hello">>, klsn_obj:normalize(Rule, "hello")),
    ?assertEqual(<<"atom">>, klsn_obj:normalize(Rule, atom)),
    ?assertEqual(<<"42">>, klsn_obj:normalize(Rule, 42)),
    ?assertMatch(<<"3.1", _/binary>>, klsn_obj:normalize(Rule, 3.14)),
    ?assertEqual({invalid, Rule, {1,2,3}}, invalid_reason(Rule, {1,2,3})),
    ?assertEqual({invalid, Rule, ['not', a, string]}, invalid_reason(Rule, ['not', a, string])).

existing_atom_rule_test() ->
    Rule = existing_atom,
    Valid = atom,
    Invalid = <<"binstr">>,
    ?assertEqual(true, klsn_obj:validate(Rule, Valid)),
    ?assertEqual(false, klsn_obj:validate(Rule, Invalid)),
    ?assertEqual(Valid, klsn_obj:normalize(Rule, Valid)),
    ?assertEqual({invalid, Rule, Invalid}, invalid_reason(Rule, Invalid)).

to_existing_atom_rule_test() ->
    Rule = to_existing_atom,
    NonExisting = crypto:strong_rand_bytes(16),
    NonExistingList = binary_to_list(NonExisting),
    ?assertEqual(atom, klsn_obj:normalize(Rule, atom)),
    ?assertEqual(atom, klsn_obj:normalize(Rule, <<"atom">>)),
    ?assertEqual(atom, klsn_obj:normalize(Rule, "atom")),
    ?assertEqual({invalid, Rule, 42}, invalid_reason(Rule, 42)),
    ?assertEqual({invalid, Rule, NonExisting}, invalid_reason(Rule, NonExisting)),
    ?assertEqual({invalid, Rule, NonExistingList}, invalid_reason(Rule, NonExistingList)).

atom_rule_test() ->
    Rule = {atom, [atom1, atom2]},
    Valid = atom2,
    Invalid = atom3,
    ?assertEqual(true, klsn_obj:validate(Rule, Valid)),
    ?assertEqual(false, klsn_obj:validate(Rule, Invalid)),
    ?assertEqual(Valid, klsn_obj:normalize(Rule, Valid)),
    ?assertEqual({invalid, Rule, Invalid}, invalid_reason(Rule, Invalid)).

to_atom_rule_test() ->
    Rule = {to_atom, [atom1, atom2]},
    Valid = atom2,
    Invalid = atom3,
    NonExisting = crypto:strong_rand_bytes(16),
    NonExistingList = binary_to_list(NonExisting),
    ?assertEqual(true, klsn_obj:validate(Rule, Valid)),
    ?assertEqual(false, klsn_obj:validate(Rule, Invalid)),
    ?assertEqual(Valid, klsn_obj:normalize(Rule, Valid)),
    ?assertEqual(Valid, klsn_obj:normalize(Rule, <<"atom2">>)),
    ?assertEqual(Valid, klsn_obj:normalize(Rule, "atom2")),
    ?assertEqual({invalid, Rule, Invalid}, invalid_reason(Rule, Invalid)),
    ?assertEqual({invalid, Rule, <<"atom3">>}, invalid_reason(Rule, <<"atom3">>)),
    ?assertEqual({invalid, Rule, "atom3"}, invalid_reason(Rule, "atom3")),
    ?assertEqual({invalid, Rule, NonExisting}, invalid_reason(Rule, NonExisting)),
    ?assertEqual({invalid, Rule, NonExistingList}, invalid_reason(Rule, NonExistingList)).

map_rule_test() ->
    Rule = #{
        required_field => {required, integer}
      , r_field => {r, integer}
      , optional_field => {optional, integer}
      , o_field => {o, integer}
      , optional_missing_field => {optional, integer}
      , o_missing_field => {o, integer}
    },
    Valid = #{
        required_field => 1
      , <<"r_field">> => 2
      , optional_field => 3
      , <<"o_field">> => 4
    },
    Expected = #{
        required_field => 1
      , r_field => 2
      , optional_field => 3
      , o_field => 4
    },
    Invalid = #{
        required_field => 1
      % <<"r_field">> => 2
      , optional_field => 3
      , <<"o_field">> => 4
    },
    ?assertEqual(true, klsn_obj:validate(Rule, Valid)),
    ?assertEqual(false, klsn_obj:validate(Rule, Invalid)),
    ?assertEqual(Expected, klsn_obj:normalize(Rule, Valid)),
    ?assertEqual({missing_required_field, r_field, Invalid}, invalid_reason(Rule, Invalid)).

map_key_value_rule_test() ->
    Rule = {map, integer, integer},
    Valid = #{
        1 => 2
      , 3 => 6
      , 4 => 8
    },
    InvalidKey = #{
        1 => 2
      , 3 => 6
      , <<"4">> => 8
    },
    InvalidValue = #{
        1 => 2
      , 3 => 6
      , 4 => <<"8">>
    },
    ?assertEqual(true, klsn_obj:validate(Rule, Valid)),
    ?assertEqual(false, klsn_obj:validate(Rule, InvalidKey)),
    ?assertEqual(false, klsn_obj:validate(Rule, InvalidValue)),
    ?assertEqual(Valid, klsn_obj:normalize(Rule, Valid)),
    ?assertEqual({invalid_map_key, {invalid, integer, <<"4">>}}, invalid_reason(Rule, InvalidKey)),
    ?assertEqual({invalid_map_value, 4, {invalid, integer, <<"8">>}}, invalid_reason(Rule, InvalidValue)).

list_rule_test() ->
    Rule = {list, integer},
    Valid = [1, 2, 3],
    Invalid = [1, 2.3, 4],
    ?assertEqual(true, klsn_obj:validate(Rule, Valid)),
    ?assertEqual(false, klsn_obj:validate(Rule, Invalid)),
    ?assertEqual(Valid, klsn_obj:normalize(Rule, Valid)),
    ?assertEqual({invalid_list_element, 2, {invalid, integer, 2.3}}, invalid_reason(Rule, Invalid)).

tuple_rule_test() ->
    Rule = {tuple, {integer, float}},
    Valid = {42, 3.14},
    Invalid = {42, 123},
    ?assertEqual(true, klsn_obj:validate(Rule, Valid)),
    ?assertEqual(false, klsn_obj:validate(Rule, Invalid)),
    ?assertEqual(Valid, klsn_obj:normalize(Rule, Valid)),
    ?assertEqual({invalid_tuple_element, 2, {invalid, float, 123}}, invalid_reason(Rule, Invalid)).

optnl_rule_test() ->
    Rule = {optnl, integer},
    Valid = {value, 42},
    Invalid = {value, 3.14},
    ?assertEqual(true, klsn_obj:validate(Rule, Valid)),
    ?assertEqual(false, klsn_obj:validate(Rule, Invalid)),
    ?assertEqual(Valid, klsn_obj:normalize(Rule, Valid)),
    ?assertEqual({invalid_optnl_value, {invalid, integer, 3.14}}, invalid_reason(Rule, Invalid)).

map_key_collision_rule_test() ->
    Rule = {map, to_integer, integer},

    Valid = #{
        <<"1">> => 10
      , "2" => 20
    },
    ?assertEqual(true, klsn_obj:validate(Rule, Valid)),
    ?assertEqual(#{1 => 10, 2 => 20}, klsn_obj:normalize(Rule, Valid)),

    Colliding1 = #{
        1 => 10
      , <<"1">> => 20
    },
    ?assertEqual(false, klsn_obj:validate(Rule, Colliding1)),
    ?assertEqual({invalid_map_key, {duplicated_map_keys, [1]}}, invalid_reason(Rule, Colliding1)),

    Colliding2 = #{
        1 => 10
      , <<"1">> => 20
      , 2 => 30
      , <<"2">> => 40
    },
    ?assertEqual(false, klsn_obj:validate(Rule, Colliding2)),
    {invalid_map_key, {duplicated_map_keys, Keys}} = invalid_reason(Rule, Colliding2),
    ?assertEqual([1, 2], lists:sort(Keys)).

timeout_rule_test() ->
    Rule = timeout,
    ValidInt = 1000,
    ValidInf = infinity,
    InvalidNeg = -1,
    InvalidType = <<"1000">>,
    ?assertEqual(true, klsn_obj:validate(Rule, ValidInt)),
    ?assertEqual(true, klsn_obj:validate(Rule, ValidInf)),
    ?assertEqual(false, klsn_obj:validate(Rule, InvalidNeg)),
    ?assertEqual(false, klsn_obj:validate(Rule, InvalidType)),
    ?assertEqual(ValidInt, klsn_obj:normalize(Rule, ValidInt)),
    ?assertEqual(ValidInf, klsn_obj:normalize(Rule, ValidInf)),
    ?assertEqual({invalid, Rule, InvalidNeg}, invalid_reason(Rule, InvalidNeg)),
    ?assertEqual({invalid, Rule, InvalidType}, invalid_reason(Rule, InvalidType)).

boolean_rule_test() ->
    Rule = boolean,
    ValidTrue = true,
    ValidFalse = false,
    Invalid = <<"true">>,
    ?assertEqual(true, klsn_obj:validate(Rule, ValidTrue)),
    ?assertEqual(true, klsn_obj:validate(Rule, ValidFalse)),
    ?assertEqual(false, klsn_obj:validate(Rule, Invalid)),
    ?assertEqual(ValidTrue, klsn_obj:normalize(Rule, ValidTrue)),
    ?assertEqual(ValidFalse, klsn_obj:normalize(Rule, ValidFalse)),
    ?assertEqual({invalid, Rule, Invalid}, invalid_reason(Rule, Invalid)).

invalid_reason(Rule, Value) ->
    try klsn_obj:normalize(Rule, Value) of
        _ ->
            error(unexpected_success)
    catch
        error:{klsn_obj, reject_reason, Reason} ->
            Reason
    end.


