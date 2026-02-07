-module(klsn_rule_annotation_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("klsn/include/klsn_rule_annotation.hrl").

-klsn_input_rule([integer, float]).
-klsn_output_rule(integer).
halve_even_or_float(0, _) ->
    <<"0">>;
halve_even_or_float(Integer, Float) ->
    case Integer rem 2 of
        0 ->
            Integer div 2;
        1 ->
            Float
    end.

halve_even_or_float_test() ->
    ?assertEqual(3, halve_even_or_float(6, 3.14)),
    ?assertEqual(3, halve_even_or_float(<<"6">>, 3.14)),
    ?assertEqual(0, halve_even_or_float(0, 3.14)),
    ?assertError(
        {klsn_input_rule, {klsn_rule_annotation_tests, halve_even_or_float, [<<"a">>, <<"b">>]}, 1, {invalid, integer, <<"a">>}},
        halve_even_or_float(<<"a">>, <<"b">>)
    ),
    ?assertError(
        {klsn_input_rule, {klsn_rule_annotation_tests, halve_even_or_float, [6, <<"b">>]}, 2, {invalid, float, <<"b">>}},
        halve_even_or_float(6, <<"b">>)
    ),
    ?assertError(
        {klsn_output_rule, {klsn_rule_annotation_tests, halve_even_or_float, [5, 3.14]}, 3.14, {invalid, integer, 3.14}},
        halve_even_or_float(5, 3.14)
    ).
