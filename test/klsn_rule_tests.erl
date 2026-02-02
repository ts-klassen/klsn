-module(klsn_rule_tests).
-include_lib("eunit/include/eunit.hrl").

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
    ).
