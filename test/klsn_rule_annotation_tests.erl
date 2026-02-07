-module(klsn_rule_annotation_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("klsn/include/klsn_rule_annotation.hrl").

-klsn_input_rule([integer, float]).
-klsn_output_rule(integer).
give_me_integer(0, _) ->
    <<"0">>;
give_me_integer(Integer, Float) ->
    case Integer rem 2 of
        0 ->
            Integer div 2;
        1 ->
            Float
    end.

give_me_integer_test() ->
    ?assertEqual(3, give_me_integer(6, 3.14)),
    ?assertEqual(3, give_me_integer(<<"6">>, 3.14)),
    ?assertEqual(0, give_me_integer(0, 3.14)),
    ?assertError(
        {klsn_input_rule, {klsn_rule_annotation_tests, give_me_integer, [<<"a">>, <<"b">>]}, 1, {invalid, integer, <<"a">>}},
        give_me_integer(<<"a">>, <<"b">>)
    ),
    ?assertError(
        {klsn_input_rule, {klsn_rule_annotation_tests, give_me_integer, [6, <<"b">>]}, 2, {invalid, float, <<"b">>}},
        give_me_integer(6, <<"b">>)
    ),
    ?assertError(
        {klsn_output_rule, {klsn_rule_annotation_tests, give_me_integer, [5, 3.14]}, 3.14, {invalid, integer, 3.14}},
        give_me_integer(5, 3.14)
    ).
