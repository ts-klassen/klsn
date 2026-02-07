-module(klsn_rule_tests).
-include_lib("eunit/include/eunit.hrl").

term_rule_test() ->
    Term = {make_ref(), self(), #{}, [], klsn_rule:module_info()},
    ?assertEqual({valid, Term}, klsn_rule:eval(term, Term)).

default_rule_test() ->
    ?assertEqual({valid, 3}, klsn_rule:eval({default, {42, integer}}, 3)),
    ?assertEqual(
        {normalized, 3, {invalid, integer, <<"3">>}}
      , klsn_rule:eval({default, {42, integer}}, <<"3">>)
    ),
    ?assertEqual(
        {normalized, 42, {invalid, integer, 3.14}}
      , klsn_rule:eval({default, {42, integer}}, 3.14)
    ).

boolean_rule_test() ->
    ?assertEqual({valid, true}, klsn_rule:eval(boolean, true)),
    ?assertEqual(
        {normalized, false, {invalid, boolean, <<"false">>}}
      , klsn_rule:eval(boolean, <<"false">>)
    ),
    ?assertEqual(
        {normalized, false, {invalid, boolean, 0}}
      , klsn_rule:eval(boolean, 0)
    ),
    ?assertEqual(
        {normalized, false, {invalid, boolean, <<>>}}
      , klsn_rule:eval(boolean, <<>>)
    ),
    ?assertEqual(
        {normalized, false, {invalid, boolean, <<"False">>}}
      , klsn_rule:eval(boolean, <<"False">>)
    ),
    ?assertEqual(
        {normalized, false, {invalid, boolean, <<"FALSE">>}}
      , klsn_rule:eval(boolean, <<"FALSE">>)
    ),
    ?assertEqual(
        {normalized, false, {invalid, boolean, <<"０"/utf8>>}}
      , klsn_rule:eval(boolean, <<"０"/utf8>>)
    ),
    ?assertEqual(
        {normalized, false, {invalid, boolean, <<"null">>}}
      , klsn_rule:eval(boolean, <<"null">>)
    ),
    ?assertEqual(
        {normalized, false, {invalid, boolean, <<"Null">>}}
      , klsn_rule:eval(boolean, <<"Null">>)
    ),
    ?assertEqual(
        {normalized, false, {invalid, boolean, <<"NULL">>}}
      , klsn_rule:eval(boolean, <<"NULL">>)
    ),
    ?assertEqual(
        {normalized, false, {invalid, boolean, <<"undefined">>}}
      , klsn_rule:eval(boolean, <<"undefined">>)
    ),
    ?assertEqual(
        {normalized, true, {invalid, boolean, <<"yes">>}}
      , klsn_rule:eval(boolean, <<"yes">>)
    ),
    ?assertEqual({reject, {invalid, boolean, {}}}, klsn_rule:eval(boolean, {})).

integer_rule_test() ->
    ?assertEqual({valid, 1}, klsn_rule:eval(integer, 1)),
    ?assertEqual(
        {normalized, 1, {invalid, integer, <<"1">>}}
      , klsn_rule:eval(integer, <<"1">>)
    ),
    ?assertEqual({reject, {invalid, integer, <<"x">>}}, klsn_rule:eval(integer, <<"x">>)).

float_rule_test() ->
    ?assertEqual({valid, 1.5}, klsn_rule:eval(float, 1.5)),
    ?assertEqual(
        {normalized, 1.5, {invalid, float, <<"1.5">>}}
      , klsn_rule:eval(float, <<"1.5">>)
    ),
    ?assertEqual({reject, {invalid, float, <<"x">>}}, klsn_rule:eval(float, <<"x">>)).

number_rule_test() ->
    ?assertEqual({valid, 1}, klsn_rule:eval(number, 1)),
    ?assertEqual(
        {normalized, 1, {invalid, number, <<"1">>}}
      , klsn_rule:eval(number, <<"1">>)
    ),
    ?assertEqual(
        {normalized, 1.5, {invalid, number, <<"1.5">>}}
      , klsn_rule:eval(number, <<"1.5">>)
    ),
    ?assertEqual({reject, {invalid, number, <<"x">>}}, klsn_rule:eval(number, <<"x">>)).

timeout_rule_test() ->
    ?assertEqual({valid, infinity}, klsn_rule:eval(timeout, infinity)),
    ?assertEqual({valid, 0}, klsn_rule:eval(timeout, 0)),
    ?assertEqual(
        {normalized, infinity, {invalid, timeout, <<"infinity">>}}
      , klsn_rule:eval(timeout, <<"infinity">>)
    ),
    ?assertEqual(
        {normalized, infinity, {invalid, timeout, "infinity"}}
      , klsn_rule:eval(timeout, "infinity")
    ),
    ?assertEqual({reject, {invalid, timeout, -1}}, klsn_rule:eval(timeout, -1)).

binstr_rule_test() ->
    ?assertEqual({valid, <<"hi">>}, klsn_rule:eval(binstr, <<"hi">>)),
    ?assertEqual(
        {normalized, <<"hi">>, {invalid, binstr, "hi"}}
      , klsn_rule:eval(binstr, "hi")
    ),
    ?assertEqual({reject, {invalid, binstr, {}}}, klsn_rule:eval(binstr, {})).

atom_rule_test() ->
    ?assertEqual({valid, ok}, klsn_rule:eval(atom, ok)),
    ?assertEqual(
        {normalized, ok, {invalid, atom, <<"ok">>}}
      , klsn_rule:eval(atom, <<"ok">>)
    ),
    ?assertEqual(
        {reject, {invalid, atom, <<"__klsn_rule_nonexistent_atom__">>}}
      , klsn_rule:eval(atom, <<"__klsn_rule_nonexistent_atom__">>)
    ).

enum_rule_test() ->
    Allowed = [a, b],
    ?assertEqual({valid, a}, klsn_rule:eval({enum, Allowed}, a)),
    ?assertEqual(
        {normalized, a, {invalid, enum, <<"a">>}}
      , klsn_rule:eval({enum, Allowed}, <<"a">>)
    ),
    ?assertEqual(
        {reject, {invalid_enum, Allowed, <<"c">>}}
      , klsn_rule:eval({enum, Allowed}, <<"c">>)
    ),
    ?assertEqual(
        {reject, {invalid_enum, Allowed, {}}}
      , klsn_rule:eval({enum, Allowed}, {})
    ),
    AllowedBad = [a, {}],
    ?assertEqual(
        {reject, {invalid_enum, AllowedBad, b}}
      , klsn_rule:eval({enum, AllowedBad}, b)
    ),
    ?assertEqual(
        {reject, {invalid, enum, a}}
      , klsn_rule:eval({enum, not_a_list}, a)
    ).

optnl_rule_test() ->
    ?assertEqual({valid, none}, klsn_rule:eval({optnl, integer}, none)),
    ?assertEqual({valid, {value, 1}}, klsn_rule:eval({optnl, integer}, {value, 1})),
    ?assertEqual(
        {normalized, {value, 1}, {invalid_optnl_value, {invalid, integer, <<"1">>}}}
      , klsn_rule:eval({optnl, integer}, {value, <<"1">>})
    ),
    ?assertEqual(
        {reject, {invalid_optnl_value, {invalid, integer, <<"x">>}}}
      , klsn_rule:eval({optnl, integer}, {value, <<"x">>})
    ),
    ?assertEqual(
        {normalized, none, {invalid, optnl, null}}
      , klsn_rule:eval({optnl, integer}, null)
    ),
    ?assertEqual(
        {normalized, none, {invalid, optnl, nil}}
      , klsn_rule:eval({optnl, integer}, nil)
    ),
    ?assertEqual(
        {normalized, none, {invalid, optnl, undefined}}
      , klsn_rule:eval({optnl, integer}, undefined)
    ),
    ?assertEqual(
        {normalized, none, {invalid, optnl, false}}
      , klsn_rule:eval({optnl, integer}, false)
    ),
    ?assertEqual(
        {normalized, none, {invalid, optnl, []}}
      , klsn_rule:eval({optnl, integer}, [])
    ),
    ?assertEqual(
        {normalized, none, {invalid, optnl, error}}
      , klsn_rule:eval({optnl, integer}, error)
    ),
    ?assertEqual(
        {normalized, {value, 1}, {invalid, optnl, {ok, 1}}}
      , klsn_rule:eval({optnl, integer}, {ok, 1})
    ),
    ?assertEqual(
        {normalized, {value, 1}, {invalid, optnl, {true, 1}}}
      , klsn_rule:eval({optnl, integer}, {true, 1})
    ),
    ?assertEqual(
        {normalized, {value, 1}, {invalid, optnl, [1]}}
      , klsn_rule:eval({optnl, integer}, [1])
    ),
    ?assertEqual(
        {normalized, {value, 1}, {invalid_optnl_value, {invalid, integer, <<"1">>}}}
      , klsn_rule:eval({optnl, integer}, <<"1">>)
    ),
    ?assertEqual(
        {normalized, {value, 123}, {invalid, optnl, 123}}
      , klsn_rule:eval({optnl, integer}, 123)
    ),
    ?assertEqual({reject, {invalid, optnl, {maybe, 1}}}, klsn_rule:eval({optnl, integer}, {maybe, 1})).

nullable_integer_rule_test() ->
    ?assertEqual({valid, null}, klsn_rule:eval({nullable, integer}, null)),
    ?assertEqual(
        {normalized, null, {invalid, nullable, none}}
      , klsn_rule:eval({nullable, integer}, none)
    ),
    ?assertEqual(
        {normalized, 1, {invalid, nullable, {value, 1}}}
      , klsn_rule:eval({nullable, integer}, {value, 1})
    ),
    ?assertEqual(
        {normalized, 1, {invalid_nullable_value, {invalid, integer, <<"1">>}}}
      , klsn_rule:eval({nullable, integer}, {value, <<"1">>})
    ),
    ?assertEqual(
        {reject, {invalid_nullable_value, {invalid, integer, <<"x">>}}}
      , klsn_rule:eval({nullable, integer}, <<"x">>)
    ).

nullable_float_rule_test() ->
    ?assertEqual({valid, null}, klsn_rule:eval({nullable, float}, null)),
    ?assertEqual(
        {normalized, null, {invalid, nullable, none}}
      , klsn_rule:eval({nullable, float}, none)
    ),
    ?assertEqual(
        {normalized, 1.0, {invalid, nullable, {value, 1.0}}}
      , klsn_rule:eval({nullable, float}, {value, 1.0})
    ),
    ?assertEqual(
        {normalized, 1.5, {invalid_nullable_value, {invalid, float, <<"1.5">>}}}
      , klsn_rule:eval({nullable, float}, {value, <<"1.5">>})
    ),
    ?assertEqual(
        {reject, {invalid_nullable_value, {invalid, float, <<"x">>}}}
      , klsn_rule:eval({nullable, float}, <<"x">>)
    ).

nullable_number_rule_test() ->
    ?assertEqual({valid, null}, klsn_rule:eval({nullable, number}, null)),
    ?assertEqual(
        {normalized, null, {invalid, nullable, none}}
      , klsn_rule:eval({nullable, number}, none)
    ),
    ?assertEqual(
        {normalized, 1, {invalid, nullable, {value, 1}}}
      , klsn_rule:eval({nullable, number}, {value, 1})
    ),
    ?assertEqual(
        {normalized, 1, {invalid_nullable_value, {invalid, number, <<"1">>}}}
      , klsn_rule:eval({nullable, number}, {value, <<"1">>})
    ),
    ?assertEqual(
        {reject, {invalid_nullable_value, {invalid, number, <<"x">>}}}
      , klsn_rule:eval({nullable, number}, <<"x">>)
    ).

nullable_binstr_rule_test() ->
    ?assertEqual({valid, null}, klsn_rule:eval({nullable, binstr}, null)),
    ?assertEqual(
        {normalized, null, {invalid, nullable, none}}
      , klsn_rule:eval({nullable, binstr}, none)
    ),
    ?assertEqual(
        {normalized, <<"a">>, {invalid, nullable, {value, <<"a">>}}}
      , klsn_rule:eval({nullable, binstr}, {value, <<"a">>})
    ),
    ?assertEqual(
        {normalized, <<"a">>, {invalid_nullable_value, {invalid, binstr, "a"}}}
      , klsn_rule:eval({nullable, binstr}, {value, "a"})
    ),
    ?assertEqual(
        {reject, {invalid_nullable_value, {invalid, binstr, {}}}}
      , klsn_rule:eval({nullable, binstr}, {})
    ).

list_rule_test() ->
    ?assertEqual({valid, [1, 2]}, klsn_rule:eval({list, integer}, [1, 2])),
    ?assertEqual(
        {normalized, [1, 2], {invalid_list_element, 1, {invalid, integer, <<"1">>}}}
      , klsn_rule:eval({list, integer}, [<<"1">>, 2])
    ),
    ?assertEqual(
        {reject, {invalid_list_element, 1, {invalid, integer, <<"x">>}}}
      , klsn_rule:eval({list, integer}, [<<"x">>])
    ),
    ?assertEqual(
        {reject, {invalid, list, #{}}}
      , klsn_rule:eval({list, integer}, #{})
    ).

tuple_rule_test() ->
    ?assertEqual({valid, {1, 2}}, klsn_rule:eval({tuple, {integer, integer}}, {1, 2})),
    ?assertEqual(
        {normalized, {1, 2}, {invalid_tuple_element, 1, {invalid, integer, <<"1">>}}}
      , klsn_rule:eval({tuple, [integer, integer]}, {<<"1">>, 2})
    ),
    ?assertEqual(
        {reject, {invalid_tuple_size, 1, {1, 2}}}
      , klsn_rule:eval({tuple, [integer]}, {1, 2})
    ),
    ?assertEqual(
        {reject, {invalid_tuple_element, 1, {invalid, integer, <<"x">>}}}
      , klsn_rule:eval({tuple, [integer]}, {<<"x">>})
    ),
    ?assertEqual(
        {reject, {invalid, tuple, [1]}}
      , klsn_rule:eval({tuple, [integer]}, [1])
    ).

map_rule_test() ->
    ?assertEqual(
        {valid, #{a => 1}}
      , klsn_rule:eval({map, {atom, integer}}, #{a => 1})
    ),
    ?assertEqual(
        {normalized, #{a => 1}, {invalid_map_key, {invalid, atom, <<"a">>}}}
      , klsn_rule:eval({map, {atom, integer}}, #{<<"a">> => 1})
    ),
    ?assertEqual(
        {normalized, #{a => 1}, {invalid_map_value, a, {invalid, integer, <<"1">>}}}
      , klsn_rule:eval({map, {atom, integer}}, #{a => <<"1">>})
    ),
    ?assertEqual(
        {reject, {invalid_map_key, {invalid, integer, a}}}
      , klsn_rule:eval({map, {integer, integer}}, #{a => 1})
    ),
    ?assertEqual(
        {reject, {invalid_map_value, a, {invalid, integer, <<"x">>}}}
      , klsn_rule:eval({map, {atom, integer}}, #{a => <<"x">>})
    ),
    ?assertEqual(
        {reject, {map_key_conflict, a}}
      , klsn_rule:eval({map, {atom, integer}}, #{a => 1, <<"a">> => 1})
    ),
    ?assertEqual(
        {reject, {map_key_conflict, a}}
      , klsn_rule:eval({map, {atom, integer}}, #{a => 1, <<"a">> => 1, <<"b">> => 2})
    ),
    ?assertEqual(
        {reject, {invalid, map, []}}
      , klsn_rule:eval({map, {atom, integer}}, [])
    ).

struct_rule_test() ->
    ?assertEqual(
        {valid, #{a => 1}}
      , klsn_rule:eval({struct, #{a => {required, integer}}}, #{a => 1})
    ),
    ?assertEqual(
        {normalized, #{a => 1}, {invalid_struct_field, <<"a">>}}
      , klsn_rule:eval({struct, #{a => {required, integer}}}, #{<<"a">> => 1})
    ),
    ?assertEqual(
        {reject, {struct_field_conflict, a}}
      , klsn_rule:eval({struct, #{a => {required, integer}}}, #{a => 1, <<"a">> => 1})
    ),
    ?assertEqual(
        {reject, {struct_field_conflict, a}}
      , klsn_rule:eval({struct, #{a => {required, integer}}}, #{"a" => 1, <<"a">> => 1})
    ),
    ?assertEqual(
        {normalized, #{a => 1}, {invalid_struct_value, a, {invalid, integer, <<"1">>}}}
      , klsn_rule:eval({struct, #{a => {required, integer}}}, #{a => <<"1">>})
    ),
    ?assertEqual(
        {normalized, #{a => 1}, {invalid_struct_field, <<"a">>}}
      , klsn_rule:eval({struct, #{a => {required, integer}}}, #{<<"a">> => <<"1">>})
    ),
    ?assertEqual(
        {reject, {missing_required_field, a}}
      , klsn_rule:eval({struct, #{a => {required, integer}}}, #{})
    ),
    ?assertEqual(
        {valid, #{}}
      , klsn_rule:eval({struct, #{a => {optional, integer}}}, #{})
    ),
    ?assertEqual(
        {normalized, #{a => 1}, {invalid_struct_field, b}}
      , klsn_rule:eval({struct, #{a => {required, integer}}}, #{a => 1, b => 2})
    ),
    ?assertEqual(
        {normalized, #{a => 1}, {invalid_struct_field, {}}}
      , klsn_rule:eval({struct, #{a => {required, integer}}}, #{a => 1, {} => 2})
    ),
    ?assertEqual(
        {reject, {invalid_struct_value, a, {invalid, integer, <<"x">>}}}
      , klsn_rule:eval({struct, #{a => {required, integer}}}, #{a => <<"x">>})
    ),
    ?assertEqual(
        {normalized, #{a => 1, b => 2}, {invalid_struct_value, a, {invalid, integer, <<"1">>}}}
      , klsn_rule:eval(
            {struct, #{a => {required, integer}, b => {required, integer}}}
          , #{a => <<"1">>, b => <<"2">>}
        )
    ),
    ?assertEqual(
        {reject, {invalid, struct, #{}}}
      , klsn_rule:eval({struct, #{<<"a">> => {required, integer}}}, #{})
    ),
    ?assertEqual(
        {reject, {invalid, struct, not_a_map}}
      , klsn_rule:eval({struct, #{a => {required, integer}}}, not_a_map)
    ).

any_of_rule_test() ->
    ?assertEqual({valid, 1}, klsn_rule:eval({any_of, []}, 1)),
    ?assertEqual({valid, 1}, klsn_rule:eval({any_of, [integer, float]}, 1)),
    ?assertEqual(
        {normalized, 1, {any_of, [{invalid, integer, <<"1">>}, {invalid, float, <<"1">>}]}}
      , klsn_rule:eval({any_of, [integer, float]}, <<"1">>)
    ),
    ?assertEqual(
        {normalized, 1, {any_of, [{invalid, float, <<"1">>}, {invalid, integer, <<"1">>}]}}
      , klsn_rule:eval({any_of, [float, integer]}, <<"1">>)
    ),
    ?assertEqual(
        {reject, {any_of, [{invalid, integer, <<"a">>}, {invalid, float, <<"a">>}]}}
      , klsn_rule:eval({any_of, [integer, float]}, <<"a">>)
    ).

all_of_rule_test() ->
    ?assertEqual({valid, 1}, klsn_rule:eval({all_of, []}, 1)),
    ?assertEqual({valid, 1}, klsn_rule:eval({all_of, [number, integer]}, 1)),
    ?assertEqual(
        {normalized, 1, {all_of, [{invalid, number, <<"1">>}, {invalid, integer, <<"1">>}]}}
      , klsn_rule:eval({all_of, [number, integer]}, <<"1">>)
    ),
    ?assertEqual(
        {normalized, <<"1">>, {all_of, [{invalid, integer, <<"1">>}]}}
      , klsn_rule:eval({all_of, [term, integer]}, <<"1">>)
    ),
    ?assertEqual(
        {normalized, <<"1">>, {all_of, [{invalid, integer, <<"1">>}]}}
      , klsn_rule:eval({all_of, [integer, term]}, <<"1">>)
    ),
    ?assertEqual(
        {reject, {all_of, [{invalid, float, <<"1">>}]}}
      , klsn_rule:eval({all_of, [number, integer, float]}, <<"1">>)
    ).

foldl_rule_test() ->
    ?assertEqual({valid, input}, klsn_rule:eval({foldl, []}, input)),
    ?assertEqual(
        {valid, 1}
      , klsn_rule:eval({foldl, [integer, number]}, 1)
    ),
    ?assertEqual(
        {normalized, 1, {invalid, integer, <<"1">>}}
      , klsn_rule:eval({foldl, [integer, {range, {number, '=<', 3}}]}, <<"1">>)
    ),
    ?assertEqual(
        {normalized, 1, {invalid, binstr, "1"}}
      , klsn_rule:eval({foldl, [binstr, integer]}, "1")
    ),
    ?assertEqual(
        {reject, {invalid_range, {1, '<', 1}}}
      , klsn_rule:eval({foldl, [integer, {range, {number, '<', 1}}]}, <<"1">>)
    ),
    ?assertEqual(
        {reject, {invalid, foldl, input}}
      , klsn_rule:eval({foldl, not_a_list}, input)
    ).

range_rule_test() ->
    ?assertEqual({valid, 3}, klsn_rule:eval({range, {number, '=<', 3}}, 3)),
    ?assertEqual(
        {normalized, 3, {invalid, number, <<"3">>}}
      , klsn_rule:eval({range, {number, '=<', 3}}, <<"3">>)
    ),
    ?assertEqual(
        {reject, {invalid_range, {3, '<', 3}}}
      , klsn_rule:eval({range, {number, '<', 3}}, 3)
    ),
    ?assertEqual(
        {valid, 2}
      , klsn_rule:eval({range, {1, '<', number}}, 2)
    ),
    ?assertEqual(
        {reject, {invalid_range, {2, '<', 1}}}
      , klsn_rule:eval({range, {2, '<', number}}, 1)
    ),
    ?assertEqual(
        {valid, 4}
      , klsn_rule:eval({range, {1, '=<', number, '=<', 5}}, 4)
    ),
    ?assertEqual(
        {reject, {invalid_range, {1, '=<', 6, '=<', 5}}}
      , klsn_rule:eval({range, {1, '=<', number, '=<', 5}}, 6)
    ),
    ?assertEqual(
        {valid, 1}
      , klsn_rule:eval({range, {1, '=<', number, '=<', 1}}, 1)
    ),
    ?assertEqual(
        {reject, {invalid_range, {1, '=<', 0, '=<', 5}}}
      , klsn_rule:eval({range, {1, '=<', number, '=<', 5}}, 0)
    ),
    ?assertEqual(
        {reject, {invalid, number, <<"x">>}}
      , klsn_rule:eval({range, {number, '<', 0}}, <<"x">>)
    ),
    ?assertEqual(
        {reject, {invalid, range, 3}}
      , klsn_rule:eval({range, {"a", '<', number}}, 3)
    ),
    ?assertEqual(
        {reject, {invalid, range, 3}}
      , klsn_rule:eval({range, {1, number, 5}}, 3)
    ).

validate_test() ->
    ?assertEqual(
        ok
      , klsn_rule:validate(
            {custom, custom_name, fun(_, Acc) -> Acc end, {valid, input}}
          , input
        )
    ),
    ?assertError(
        {klsn_rule, {custom, normalized_reason}}
      , klsn_rule:validate(
            {custom, custom_name, fun(_, Acc) -> Acc end, {normalized, output, {custom, normalized_reason}}}
          , input
        )
    ),
    ?assertError(
        {klsn_rule, {custom, reject_reason}}
      , klsn_rule:validate(
            {custom, custom_name, fun(_, Acc) -> Acc end, {reject, {custom, reject_reason}}}
          , input
        )
    ).

normalize_test() ->
    ?assertEqual(
        input
      , klsn_rule:normalize(
            {custom, custom_name, fun(_, Acc) -> Acc end, {valid, input}}
          , input
        )
    ),
    ?assertEqual(
        output
      , klsn_rule:normalize(
            {custom, custom_name, fun(_, Acc) -> Acc end, {normalized, output, {custom, normalized_reason}}}
          , input
        )
    ),
    ?assertError(
        {klsn_rule, {custom, reject_reason}}
      , klsn_rule:normalize(
            {custom, custom_name, fun(_, Acc) -> Acc end, {reject, {custom, reject_reason}}}
          , input
        )
    ).

eval_test() ->
    ?assertEqual(
        {valid, input}
      , klsn_rule:eval(
            {custom, custom_name, fun(_, _) -> valid end, ignored_acc}
          , input
        )
    ),
    ?assertEqual(
        {valid, input}
      , klsn_rule:eval(
            {custom, custom_name, fun(_, Acc) -> Acc end, {valid, input}}
          , input
        )
    ),
    ?assertException(
        error
      , {invalid_custom_rule, _}
      , klsn_rule:eval(
            {custom, custom_name, fun(_, Acc) -> Acc end, {valid, modified}}
          , input
        )
    ),
    ?assertEqual(
        {normalized, output, {invalid, custom_name, input}}
      , klsn_rule:eval(
            {custom, custom_name, fun(_, Acc) -> Acc end, {normalized, output}}
          , input
        )
    ),
    ?assertEqual(
        {normalized, output, {custom, explicit_reason}}
      , klsn_rule:eval(
            {custom, custom_name, fun(_, Acc) -> Acc end, {normalized, output, {custom, explicit_reason}}}
          , input
        )
    ),
    ?assertEqual(
        {reject, {invalid, custom_name, input}}
      , klsn_rule:eval(
            {custom, custom_name, fun(_, _) -> reject end, ignored_acc}
          , input
        )
    ),
    ?assertEqual(
        {reject, {custom, explicit_reject_reason}}
      , klsn_rule:eval(
            {custom, custom_name, fun(_, Acc) -> Acc end, {reject, {custom, explicit_reject_reason}}}
          , input
        )
    ),
    ?assertException(
        error
      , {invalid_custom_rule, _}
      , klsn_rule:eval(
            {custom, custom_name, fun(_, _) -> unexpected end, ignored_acc}
          , input
        )
    ),
    ?assertEqual(
        {reject, {unknown_rule, unknown_rule_name}}
      , klsn_rule:eval(unknown_rule_name, input)
    ).
