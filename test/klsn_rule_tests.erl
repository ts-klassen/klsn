-module(klsn_rule_tests).
-include_lib("eunit/include/eunit.hrl").

-klsn_rule_alias([
        {my_alias1, integer}
      , {my_alias2, float}
    ]).

-klsn_rule_alias([
        {my_alias3, binstr}
      , {my_alias4, {optnl, {alias, {klsn_rule_tests, my_alias1}}}}
    ]).

-klsn_rule_alias({my_alias5, boolean}).

term_rule_test() ->
    Term = {make_ref(), self(), #{}, [], klsn_rule:module_info()},
    ?assertEqual({valid, Term}, klsn_rule:eval(Term, term, #{})).

exact_rule_test() ->
    ?assertEqual({valid, my_value}, klsn_rule:eval(my_value, {exact, my_value}, #{})),
    ?assertEqual(
        {reject, {invalid_exact, my_value, other_value}}
      , klsn_rule:eval(other_value, {exact, my_value}, #{})
    ).

default_rule_test() ->
    ?assertEqual({valid, 3}, klsn_rule:eval(3, {default, {42, integer}}, #{})),
    ?assertEqual(
        {normalized, 3, {invalid, integer, <<"3">>}}
      , klsn_rule:eval(<<"3">>, {default, {42, integer}}, #{})
    ),
    ?assertEqual(
        {normalized, 42, {invalid, integer, 3.14}}
      , klsn_rule:eval(3.14, {default, {42, integer}}, #{})
    ).

boolean_rule_test() ->
    ?assertEqual({valid, true}, klsn_rule:eval(true, boolean, #{})),
    ?assertEqual(
        {normalized, false, {invalid, boolean, <<"false">>}}
      , klsn_rule:eval(<<"false">>, boolean, #{})
    ),
    ?assertEqual(
        {normalized, false, {invalid, boolean, 0}}
      , klsn_rule:eval(0, boolean, #{})
    ),
    ?assertEqual(
        {normalized, false, {invalid, boolean, <<>>}}
      , klsn_rule:eval(<<>>, boolean, #{})
    ),
    ?assertEqual(
        {normalized, false, {invalid, boolean, <<"False">>}}
      , klsn_rule:eval(<<"False">>, boolean, #{})
    ),
    ?assertEqual(
        {normalized, false, {invalid, boolean, <<"FALSE">>}}
      , klsn_rule:eval(<<"FALSE">>, boolean, #{})
    ),
    ?assertEqual(
        {normalized, false, {invalid, boolean, <<"０"/utf8>>}}
      , klsn_rule:eval(<<"０"/utf8>>, boolean, #{})
    ),
    ?assertEqual(
        {normalized, false, {invalid, boolean, <<"null">>}}
      , klsn_rule:eval(<<"null">>, boolean, #{})
    ),
    ?assertEqual(
        {normalized, false, {invalid, boolean, <<"Null">>}}
      , klsn_rule:eval(<<"Null">>, boolean, #{})
    ),
    ?assertEqual(
        {normalized, false, {invalid, boolean, <<"NULL">>}}
      , klsn_rule:eval(<<"NULL">>, boolean, #{})
    ),
    ?assertEqual(
        {normalized, false, {invalid, boolean, <<"undefined">>}}
      , klsn_rule:eval(<<"undefined">>, boolean, #{})
    ),
    ?assertEqual(
        {normalized, true, {invalid, boolean, <<"yes">>}}
      , klsn_rule:eval(<<"yes">>, boolean, #{})
    ),
    ?assertEqual({reject, {invalid, boolean, {}}}, klsn_rule:eval({}, boolean, #{})).

integer_rule_test() ->
    ?assertEqual({valid, 1}, klsn_rule:eval(1, integer, #{})),
    ?assertEqual(
        {normalized, 1, {invalid, integer, <<"1">>}}
      , klsn_rule:eval(<<"1">>, integer, #{})
    ),
    ?assertEqual({reject, {invalid, integer, <<"x">>}}, klsn_rule:eval(<<"x">>, integer, #{})).

float_rule_test() ->
    ?assertEqual({valid, 1.5}, klsn_rule:eval(1.5, float, #{})),
    ?assertEqual(
        {normalized, 1.5, {invalid, float, <<"1.5">>}}
      , klsn_rule:eval(<<"1.5">>, float, #{})
    ),
    ?assertEqual({reject, {invalid, float, <<"x">>}}, klsn_rule:eval(<<"x">>, float, #{})).

number_rule_test() ->
    ?assertEqual({valid, 1}, klsn_rule:eval(1, number, #{})),
    ?assertEqual(
        {normalized, 1, {invalid, number, <<"1">>}}
      , klsn_rule:eval(<<"1">>, number, #{})
    ),
    ?assertEqual(
        {normalized, 1.5, {invalid, number, <<"1.5">>}}
      , klsn_rule:eval(<<"1.5">>, number, #{})
    ),
    ?assertEqual({reject, {invalid, number, <<"x">>}}, klsn_rule:eval(<<"x">>, number, #{})).

timeout_rule_test() ->
    ?assertEqual({valid, infinity}, klsn_rule:eval(infinity, timeout, #{})),
    ?assertEqual({valid, 0}, klsn_rule:eval(0, timeout, #{})),
    ?assertEqual(
        {normalized, infinity, {invalid, timeout, <<"infinity">>}}
      , klsn_rule:eval(<<"infinity">>, timeout, #{})
    ),
    ?assertEqual(
        {normalized, infinity, {invalid, timeout, "infinity"}}
      , klsn_rule:eval("infinity", timeout, #{})
    ),
    ?assertEqual(
        {reject, {invalid, timeout, <<"1.5">>}}
      , klsn_rule:eval(<<"1.5">>, timeout, #{})
    ),
    ?assertEqual(
        {reject, {invalid, timeout, <<"-1">>}}
      , klsn_rule:eval(<<"-1">>, timeout, #{})
    ),
    ?assertEqual(
        {reject, {invalid, timeout, "-1"}}
      , klsn_rule:eval("-1", timeout, #{})
    ),
    ?assertEqual({reject, {invalid, timeout, -1}}, klsn_rule:eval(-1, timeout, #{})).

binstr_rule_test() ->
    ?assertEqual({valid, <<"hi">>}, klsn_rule:eval(<<"hi">>, binstr, #{})),
    ?assertEqual(
        {normalized, <<"hi">>, {invalid, binstr, "hi"}}
      , klsn_rule:eval("hi", binstr, #{})
    ),
    ?assertEqual({reject, {invalid, binstr, {}}}, klsn_rule:eval({}, binstr, #{})).

atom_rule_test() ->
    ?assertEqual({valid, ok}, klsn_rule:eval(ok, atom, #{})),
    ?assertEqual(
        {normalized, ok, {invalid, atom, <<"ok">>}}
      , klsn_rule:eval(<<"ok">>, atom, #{})
    ),
    ?assertEqual(
        {reject, {invalid, atom, <<"__klsn_rule_nonexistent_atom__">>}}
      , klsn_rule:eval(<<"__klsn_rule_nonexistent_atom__">>, atom, #{})
    ).

enum_rule_test() ->
    Allowed = [a, b],
    ?assertEqual({valid, a}, klsn_rule:eval(a, {enum, Allowed}, #{})),
    ?assertEqual(
        {normalized, a, {invalid, enum, <<"a">>}}
      , klsn_rule:eval(<<"a">>, {enum, Allowed}, #{})
    ),
    ?assertEqual(
        {reject, {invalid_enum, Allowed, <<"c">>}}
      , klsn_rule:eval(<<"c">>, {enum, Allowed}, #{})
    ),
    ?assertEqual(
        {reject, {invalid_enum, Allowed, {}}}
      , klsn_rule:eval({}, {enum, Allowed}, #{})
    ),
    AllowedBad = [a, {}],
    ?assertEqual(
        {reject, {invalid_enum, AllowedBad, b}}
      , klsn_rule:eval(b, {enum, AllowedBad}, #{})
    ),
    ?assertEqual(
        {reject, {invalid, enum, a}}
      , klsn_rule:eval(a, {enum, not_a_list}, #{})
    ).

optnl_rule_test() ->
    ?assertEqual({valid, none}, klsn_rule:eval(none, {optnl, integer}, #{})),
    ?assertEqual({valid, {value, 1}}, klsn_rule:eval({value, 1}, {optnl, integer}, #{})),
    ?assertEqual(
        {normalized, {value, 1}, {invalid_optnl_value, {invalid, integer, <<"1">>}}}
      , klsn_rule:eval({value, <<"1">>}, {optnl, integer}, #{})
    ),
    ?assertEqual(
        {reject, {invalid_optnl_value, {invalid, integer, <<"x">>}}}
      , klsn_rule:eval({value, <<"x">>}, {optnl, integer}, #{})
    ),
    ?assertEqual(
        {normalized, none, {invalid, optnl, null}}
      , klsn_rule:eval(null, {optnl, integer}, #{})
    ),
    ?assertEqual(
        {normalized, none, {invalid, optnl, nil}}
      , klsn_rule:eval(nil, {optnl, integer}, #{})
    ),
    ?assertEqual(
        {normalized, none, {invalid, optnl, undefined}}
      , klsn_rule:eval(undefined, {optnl, integer}, #{})
    ),
    ?assertEqual(
        {normalized, none, {invalid, optnl, false}}
      , klsn_rule:eval(false, {optnl, integer}, #{})
    ),
    ?assertEqual(
        {normalized, none, {invalid, optnl, []}}
      , klsn_rule:eval([], {optnl, integer}, #{})
    ),
    ?assertEqual(
        {normalized, none, {invalid, optnl, error}}
      , klsn_rule:eval(error, {optnl, integer}, #{})
    ),
    ?assertEqual(
        {normalized, {value, 1}, {invalid, optnl, {ok, 1}}}
      , klsn_rule:eval({ok, 1}, {optnl, integer}, #{})
    ),
    ?assertEqual(
        {normalized, {value, 1}, {invalid, optnl, {true, 1}}}
      , klsn_rule:eval({true, 1}, {optnl, integer}, #{})
    ),
    ?assertEqual(
        {normalized, {value, 1}, {invalid, optnl, [1]}}
      , klsn_rule:eval([1], {optnl, integer}, #{})
    ),
    ?assertEqual(
        {normalized, {value, 1}, {invalid_optnl_value, {invalid, integer, <<"1">>}}}
      , klsn_rule:eval(<<"1">>, {optnl, integer}, #{})
    ),
    ?assertEqual(
        {normalized, {value, 123}, {invalid, optnl, 123}}
      , klsn_rule:eval(123, {optnl, integer}, #{})
    ),
    ?assertEqual({reject, {invalid, optnl, {my_maybe_monad, 1}}}, klsn_rule:eval({my_maybe_monad, 1}, {optnl, integer}, #{})).

nullable_integer_rule_test() ->
    ?assertEqual({valid, null}, klsn_rule:eval(null, {nullable, integer}, #{})),
    ?assertEqual(
        {normalized, null, {invalid, nullable, none}}
      , klsn_rule:eval(none, {nullable, integer}, #{})
    ),
    ?assertEqual(
        {normalized, 1, {invalid, nullable, {value, 1}}}
      , klsn_rule:eval({value, 1}, {nullable, integer}, #{})
    ),
    ?assertEqual(
        {normalized, 1, {invalid_nullable_value, {invalid, integer, <<"1">>}}}
      , klsn_rule:eval({value, <<"1">>}, {nullable, integer}, #{})
    ),
    ?assertEqual(
        {reject, {invalid_nullable_value, {invalid, integer, <<"x">>}}}
      , klsn_rule:eval(<<"x">>, {nullable, integer}, #{})
    ).

nullable_float_rule_test() ->
    ?assertEqual({valid, null}, klsn_rule:eval(null, {nullable, float}, #{})),
    ?assertEqual(
        {normalized, null, {invalid, nullable, none}}
      , klsn_rule:eval(none, {nullable, float}, #{})
    ),
    ?assertEqual(
        {normalized, 1.0, {invalid, nullable, {value, 1.0}}}
      , klsn_rule:eval({value, 1.0}, {nullable, float}, #{})
    ),
    ?assertEqual(
        {normalized, 1.5, {invalid_nullable_value, {invalid, float, <<"1.5">>}}}
      , klsn_rule:eval({value, <<"1.5">>}, {nullable, float}, #{})
    ),
    ?assertEqual(
        {reject, {invalid_nullable_value, {invalid, float, <<"x">>}}}
      , klsn_rule:eval(<<"x">>, {nullable, float}, #{})
    ).

nullable_number_rule_test() ->
    ?assertEqual({valid, null}, klsn_rule:eval(null, {nullable, number}, #{})),
    ?assertEqual(
        {normalized, null, {invalid, nullable, none}}
      , klsn_rule:eval(none, {nullable, number}, #{})
    ),
    ?assertEqual(
        {normalized, 1, {invalid, nullable, {value, 1}}}
      , klsn_rule:eval({value, 1}, {nullable, number}, #{})
    ),
    ?assertEqual(
        {normalized, 1, {invalid_nullable_value, {invalid, number, <<"1">>}}}
      , klsn_rule:eval({value, <<"1">>}, {nullable, number}, #{})
    ),
    ?assertEqual(
        {reject, {invalid_nullable_value, {invalid, number, <<"x">>}}}
      , klsn_rule:eval(<<"x">>, {nullable, number}, #{})
    ).

nullable_binstr_rule_test() ->
    ?assertEqual({valid, null}, klsn_rule:eval(null, {nullable, binstr}, #{})),
    ?assertEqual(
        {normalized, null, {invalid, nullable, none}}
      , klsn_rule:eval(none, {nullable, binstr}, #{})
    ),
    ?assertEqual(
        {normalized, <<"a">>, {invalid, nullable, {value, <<"a">>}}}
      , klsn_rule:eval({value, <<"a">>}, {nullable, binstr}, #{})
    ),
    ?assertEqual(
        {normalized, <<"a">>, {invalid_nullable_value, {invalid, binstr, "a"}}}
      , klsn_rule:eval({value, "a"}, {nullable, binstr}, #{})
    ),
    ?assertEqual(
        {reject, {invalid_nullable_value, {invalid, binstr, {}}}}
      , klsn_rule:eval({}, {nullable, binstr}, #{})
    ).

strict_rule_test() ->
    ?assertEqual({valid, 1}, klsn_rule:eval(1, {strict, integer}, #{})),
    ?assertEqual(
        {reject, {strict, {invalid, integer, <<"1">>}}}
      , klsn_rule:eval(<<"1">>, {strict, integer}, #{})
    ),
    ?assertEqual(
        {reject, {invalid, integer, <<"x">>}}
      , klsn_rule:eval(<<"x">>, {strict, integer}, #{})
    ).

list_rule_test() ->
    ?assertEqual({valid, [1, 2]}, klsn_rule:eval([1, 2], {list, integer}, #{})),
    ?assertEqual(
        {normalized, [1, 2], {invalid_list_element, 1, {invalid, integer, <<"1">>}}}
      , klsn_rule:eval([<<"1">>, 2], {list, integer}, #{})
    ),
    ?assertEqual(
        {reject, {invalid_list_element, 1, {invalid, integer, <<"x">>}}}
      , klsn_rule:eval([<<"x">>], {list, integer}, #{})
    ),
    ?assertEqual(
        {reject, {invalid, list, #{}}}
      , klsn_rule:eval(#{}, {list, integer}, #{})
    ).

tuple_rule_test() ->
    ?assertEqual({valid, {1, 2}}, klsn_rule:eval({1, 2}, {tuple, {integer, integer}}, #{})),
    ?assertEqual(
        {normalized, {1, 2}, {invalid_tuple_element, 1, {invalid, integer, <<"1">>}}}
      , klsn_rule:eval({<<"1">>, 2}, {tuple, [integer, integer]}, #{})
    ),
    ?assertEqual(
        {reject, {invalid_tuple_size, 1, {1, 2}}}
      , klsn_rule:eval({1, 2}, {tuple, [integer]}, #{})
    ),
    ?assertEqual(
        {reject, {invalid_tuple_element, 1, {invalid, integer, <<"x">>}}}
      , klsn_rule:eval({<<"x">>}, {tuple, [integer]}, #{})
    ),
    ?assertEqual(
        {reject, {invalid, tuple, [1]}}
      , klsn_rule:eval([1], {tuple, [integer]}, #{})
    ).

map_rule_test() ->
    ?assertEqual(
        {valid, #{a => 1}}
      , klsn_rule:eval(#{a => 1}, {map, {atom, integer}}, #{})
    ),
    ?assertEqual(
        {normalized, #{a => 1}, {invalid_map_key, {invalid, atom, <<"a">>}}}
      , klsn_rule:eval(#{<<"a">> => 1}, {map, {atom, integer}}, #{})
    ),
    ?assertEqual(
        {normalized, #{a => 1}, {invalid_map_value, a, {invalid, integer, <<"1">>}}}
      , klsn_rule:eval(#{a => <<"1">>}, {map, {atom, integer}}, #{})
    ),
    ?assertEqual(
        {reject, {invalid_map_key, {invalid, integer, a}}}
      , klsn_rule:eval(#{a => 1}, {map, {integer, integer}}, #{})
    ),
    ?assertEqual(
        {reject, {invalid_map_value, a, {invalid, integer, <<"x">>}}}
      , klsn_rule:eval(#{a => <<"x">>}, {map, {atom, integer}}, #{})
    ),
    ?assertEqual(
        {reject, {map_key_conflict, a}}
      , klsn_rule:eval(#{a => 1, <<"a">> => 1}, {map, {atom, integer}}, #{})
    ),
    ?assertEqual(
        {reject, {map_key_conflict, a}}
      , klsn_rule:eval(#{a => 1, <<"a">> => 1, <<"b">> => 2}, {map, {atom, integer}}, #{})
    ),
    ?assertEqual(
        {reject, {invalid, map, []}}
      , klsn_rule:eval([], {map, {atom, integer}}, #{})
    ).

struct_rule_test() ->
    ?assertEqual(
        {valid, #{a => 1}}
      , klsn_rule:eval(#{a => 1}, {struct, #{a => {required, integer}}}, #{})
    ),
    ?assertEqual(
        {normalized, #{a => 1}, {invalid_struct_field, <<"a">>}}
      , klsn_rule:eval(#{<<"a">> => 1}, {struct, #{a => {required, integer}}}, #{})
    ),
    ?assertEqual(
        {reject, {struct_field_conflict, a}}
      , klsn_rule:eval(#{a => 1, <<"a">> => 1}, {struct, #{a => {required, integer}}}, #{})
    ),
    ?assertEqual(
        {reject, {struct_field_conflict, a}}
      , klsn_rule:eval(#{"a" => 1, <<"a">> => 1}, {struct, #{a => {required, integer}}}, #{})
    ),
    ?assertEqual(
        {normalized, #{a => 1}, {invalid_struct_value, a, {invalid, integer, <<"1">>}}}
      , klsn_rule:eval(#{a => <<"1">>}, {struct, #{a => {required, integer}}}, #{})
    ),
    ?assertEqual(
        {normalized, #{a => 1}, {invalid_struct_field, <<"a">>}}
      , klsn_rule:eval(#{<<"a">> => <<"1">>}, {struct, #{a => {required, integer}}}, #{})
    ),
    ?assertEqual(
        {reject, {missing_required_field, a}}
      , klsn_rule:eval(#{}, {struct, #{a => {required, integer}}}, #{})
    ),
    ?assertEqual(
        {valid, #{}}
      , klsn_rule:eval(#{}, {struct, #{a => {optional, integer}}}, #{})
    ),
    ?assertEqual(
        {normalized, #{a => 1}, {invalid_struct_field, b}}
      , klsn_rule:eval(#{a => 1, b => 2}, {struct, #{a => {required, integer}}}, #{})
    ),
    ?assertEqual(
        {normalized, #{a => 1}, {invalid_struct_field, {}}}
      , klsn_rule:eval(#{a => 1, {} => 2}, {struct, #{a => {required, integer}}}, #{})
    ),
    ?assertEqual(
        {reject, {invalid_struct_value, a, {invalid, integer, <<"x">>}}}
      , klsn_rule:eval(#{a => <<"x">>}, {struct, #{a => {required, integer}}}, #{})
    ),
    ?assertEqual(
        {normalized, #{a => 1, b => 2}, {invalid_struct_value, a, {invalid, integer, <<"1">>}}}
      , klsn_rule:eval(
            #{a => <<"1">>, b => <<"2">>}
          , {struct, #{a => {required, integer}, b => {required, integer}}}
          , #{}
        )
    ),
    ?assertEqual(
        {reject, {invalid, struct, #{}}}
      , klsn_rule:eval(#{}, {struct, #{<<"a">> => {required, integer}}}, #{})
    ),
    ?assertEqual(
        {reject, {invalid, struct, not_a_map}}
      , klsn_rule:eval(not_a_map, {struct, #{a => {required, integer}}}, #{})
    ).

any_of_rule_test() ->
    ?assertEqual({valid, 1}, klsn_rule:eval(1, {any_of, []}, #{})),
    ?assertEqual({valid, 1}, klsn_rule:eval(1, {any_of, [integer, float]}, #{})),
    ?assertEqual(
        {normalized, 1, {any_of, [{invalid, integer, <<"1">>}, {invalid, float, <<"1">>}]}}
      , klsn_rule:eval(<<"1">>, {any_of, [integer, float]}, #{})
    ),
    ?assertEqual(
        {normalized, 1, {any_of, [{invalid, float, <<"1">>}, {invalid, integer, <<"1">>}]}}
      , klsn_rule:eval(<<"1">>, {any_of, [float, integer]}, #{})
    ),
    ?assertEqual(
        {reject, {any_of, [{invalid, integer, <<"a">>}, {invalid, float, <<"a">>}]}}
      , klsn_rule:eval(<<"a">>, {any_of, [integer, float]}, #{})
    ).

all_of_rule_test() ->
    ?assertEqual({valid, 1}, klsn_rule:eval(1, {all_of, []}, #{})),
    ?assertEqual({valid, 1}, klsn_rule:eval(1, {all_of, [number, integer]}, #{})),
    ?assertEqual(
        {normalized, 1, {all_of, [{invalid, number, <<"1">>}, {invalid, integer, <<"1">>}]}}
      , klsn_rule:eval(<<"1">>, {all_of, [number, integer]}, #{})
    ),
    ?assertEqual(
        {normalized, <<"1">>, {all_of, [{invalid, integer, <<"1">>}]}}
      , klsn_rule:eval(<<"1">>, {all_of, [term, integer]}, #{})
    ),
    ?assertEqual(
        {normalized, <<"1">>, {all_of, [{invalid, integer, <<"1">>}]}}
      , klsn_rule:eval(<<"1">>, {all_of, [integer, term]}, #{})
    ),
    ?assertEqual(
        {reject, {all_of, [{invalid, float, <<"1">>}]}}
      , klsn_rule:eval(<<"1">>, {all_of, [number, integer, float]}, #{})
    ).

foldl_rule_test() ->
    ?assertEqual({valid, input}, klsn_rule:eval(input, {foldl, []}, #{})),
    ?assertEqual(
        {valid, 1}
      , klsn_rule:eval(1, {foldl, [integer, number]}, #{})
    ),
    ?assertEqual(
        {normalized, 1, {invalid, integer, <<"1">>}}
      , klsn_rule:eval(<<"1">>, {foldl, [integer, {range, {number, '=<', 3}}]}, #{})
    ),
    ?assertEqual(
        {normalized, 1, {invalid, binstr, "1"}}
      , klsn_rule:eval("1", {foldl, [binstr, integer]}, #{})
    ),
    ?assertEqual(
        {reject, {invalid_range, {1, '<', 1}}}
      , klsn_rule:eval(<<"1">>, {foldl, [integer, {range, {number, '<', 1}}]}, #{})
    ),
    ?assertEqual(
        {reject, {invalid, foldl, input}}
      , klsn_rule:eval(input, {foldl, not_a_list}, #{})
    ).

range_rule_test() ->
    ?assertEqual({valid, 3}, klsn_rule:eval(3, {range, {number, '=<', 3}}, #{})),
    ?assertEqual(
        {normalized, 3, {invalid, number, <<"3">>}}
      , klsn_rule:eval(<<"3">>, {range, {number, '=<', 3}}, #{})
    ),
    ?assertEqual(
        {reject, {invalid_range, {3, '<', 3}}}
      , klsn_rule:eval(3, {range, {number, '<', 3}}, #{})
    ),
    ?assertEqual(
        {valid, 2}
      , klsn_rule:eval(2, {range, {1, '<', number}}, #{})
    ),
    ?assertEqual(
        {reject, {invalid_range, {2, '<', 1}}}
      , klsn_rule:eval(1, {range, {2, '<', number}}, #{})
    ),
    ?assertEqual(
        {valid, 4}
      , klsn_rule:eval(4, {range, {1, '=<', number, '=<', 5}}, #{})
    ),
    ?assertEqual(
        {reject, {invalid_range, {1, '=<', 6, '=<', 5}}}
      , klsn_rule:eval(6, {range, {1, '=<', number, '=<', 5}}, #{})
    ),
    ?assertEqual(
        {valid, 1}
      , klsn_rule:eval(1, {range, {1, '=<', number, '=<', 1}}, #{})
    ),
    ?assertEqual(
        {reject, {invalid_range, {1, '=<', 0, '=<', 5}}}
      , klsn_rule:eval(0, {range, {1, '=<', number, '=<', 5}}, #{})
    ),
    ?assertEqual(
        {reject, {invalid, number, <<"x">>}}
      , klsn_rule:eval(<<"x">>, {range, {number, '<', 0}}, #{})
    ),
    ?assertEqual(
        {reject, {invalid, range, 3}}
      , klsn_rule:eval(3, {range, {"a", '<', number}}, #{})
    ),
    ?assertEqual(
        {reject, {invalid, range, 3}}
      , klsn_rule:eval(3, {range, {1, number, 5}}, #{})
    ).

klsn_rule_alias_test() ->
    MyAlias1 = {klsn_rule_tests, my_alias1},
    MyAlias4 = {klsn_rule_tests, my_alias4},
    ?assertEqual(
        {valid, 42}
      , klsn_rule:eval(42, {alias, MyAlias1}, #{})
    ),
    ?assertEqual(
        {normalized, 42, {invalid_alias, MyAlias1, {invalid, integer, <<"42">>}}}
      , klsn_rule:eval(<<"42">>, {alias, MyAlias1}, #{})
    ),
    ?assertEqual(
        {reject, {invalid_alias, MyAlias1, {invalid, integer, <<"a">>}}}
      , klsn_rule:eval(<<"a">>, {alias, MyAlias1}, #{})
    ),
    ?assertEqual(
        {valid, {value, 42}}
      , klsn_rule:eval({value, 42}, {alias, MyAlias4}, #{})
    ),
    ?assertEqual(
        {normalized, {value, 42}, {invalid_alias, MyAlias4, {invalid_optnl_value, {invalid_alias, MyAlias1, {invalid, integer, <<"42">>}}}}}
      , klsn_rule:eval({value, <<"42">>}, {alias, MyAlias4}, #{})
    ),
    ?assertEqual(
        {reject, {invalid_alias, MyAlias4, {invalid_optnl_value, {invalid_alias, MyAlias1, {invalid, integer, <<"a">>}}}}}
      , klsn_rule:eval({value, <<"a">>}, {alias, MyAlias4}, #{})
    ),
    ?assertEqual(
        {reject, {undefined_alias, {non_a_module, missing_alias}, 42}}
      , klsn_rule:eval(42, {alias, {non_a_module, missing_alias}}, #{})
    ),
    ?assertEqual(
        {reject, {invalid, alias, 42}}
      , klsn_rule:eval(42, {alias, bad_alias}, #{})
    ).

lookup_alias_test() ->
    ?assertEqual(
        {value, integer}
      , klsn_rule:lookup_alias({klsn_rule_tests, my_alias1})
    ),
    ?assertEqual(
        {value, float}
      , klsn_rule:lookup_alias({klsn_rule_tests, my_alias2})
    ),
    ?assertEqual(
        {value, binstr}
      , klsn_rule:lookup_alias({klsn_rule_tests, my_alias3})
    ),
    ?assertEqual(
        {value, {optnl, {alias, {klsn_rule_tests, my_alias1}}}}
      , klsn_rule:lookup_alias({klsn_rule_tests, my_alias4})
    ),
    ?assertEqual(
        {value, boolean}
      , klsn_rule:lookup_alias({klsn_rule_tests, my_alias5})
    ),
    ?assertEqual(
        none
      , klsn_rule:lookup_alias({klsn_rule_tests, my_alias6})
    ),
    ?assertEqual(
        none
      , klsn_rule:lookup_alias(invalid_input)
    ).

validate_test() ->
    ?assertEqual(
        ok
      , klsn_rule:validate(
            input
          , {custom, custom_name, fun(_, Param, _) -> Param end, {valid, input}}
        )
    ),
    ?assertError(
        {klsn_rule, {custom, normalized_reason}}
      , klsn_rule:validate(
            input
          , {custom, custom_name, fun(_, Param, _) -> Param end, {normalized, output, {custom, normalized_reason}}}
        )
    ),
    ?assertError(
        {klsn_rule, {custom, reject_reason}}
      , klsn_rule:validate(
            input
          , {custom, custom_name, fun(_, Param, _) -> Param end, {reject, {custom, reject_reason}}}
        )
    ).

normalize_test() ->
    ?assertEqual(
        input
      , klsn_rule:normalize(
            input
          , {custom, custom_name, fun(_, Param, _) -> Param end, {valid, input}}
        )
    ),
    ?assertEqual(
        output
      , klsn_rule:normalize(
            input
          , {custom, custom_name, fun(_, Param, _) -> Param end, {normalized, output, {custom, normalized_reason}}}
        )
    ),
    ?assertError(
        {klsn_rule, {custom, reject_reason}}
      , klsn_rule:normalize(
            input
          , {custom, custom_name, fun(_, Param, _) -> Param end, {reject, {custom, reject_reason}}}
        )
    ).

eval_test() ->
    ?assertEqual(
        {valid, input}
      , klsn_rule:eval(
            input
          , {custom, custom_name, fun(_, _, _) -> valid end, ignored_acc}
          , #{}
        )
    ),
    ?assertEqual(
        {valid, input}
      , klsn_rule:eval(
            input
          , {custom, custom_name, fun(_, Param, _) -> Param end, {valid, input}}
          , #{}
        )
    ),
    ?assertException(
        error
      , {invalid_custom_rule, _}
      , klsn_rule:eval(
            input
          , {custom, custom_name, fun(_, Param, _) -> Param end, {valid, modified}}
          , #{}
        )
    ),
    ?assertEqual(
        {normalized, output, {invalid, custom_name, input}}
      , klsn_rule:eval(
            input
          , {custom, custom_name, fun(_, Param, _) -> Param end, {normalized, output}}
          , #{}
        )
    ),
    ?assertEqual(
        {normalized, output, {custom, explicit_reason}}
      , klsn_rule:eval(
            input
          , {custom, custom_name, fun(_, Param, _) -> Param end, {normalized, output, {custom, explicit_reason}}}
          , #{}
        )
    ),
    ?assertEqual(
        {reject, {invalid, custom_name, input}}
      , klsn_rule:eval(
            input
          , {custom, custom_name, fun(_, _, _) -> reject end, ignored_acc}
          , #{}
        )
    ),
    ?assertEqual(
        {reject, {custom, explicit_reject_reason}}
      , klsn_rule:eval(
            input
          , {custom, custom_name, fun(_, Param, _) -> Param end, {reject, {custom, explicit_reject_reason}}}
          , #{}
        )
    ),
    ?assertException(
        error
      , {invalid_custom_rule, _}
      , klsn_rule:eval(
            input
          , {custom, custom_name, fun(_, _, _) -> unexpected end, ignored_acc}
          , #{}
        )
    ),
    ?assertEqual(
        {reject, {unknown_rule, unknown_rule_name}}
      , klsn_rule:eval(input, unknown_rule_name, #{})
    ).
