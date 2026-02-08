-module(klsn_rule_generator_tests).
-include_lib("eunit/include/eunit.hrl").

integer_schema_generates_integer_rule_test() ->
    Schema = #{<<"type">> => <<"integer">>},
    #{from_json := FromJsonRule, to_json := ToJsonRule} = klsn_rule_generator:from_json_schema(Schema),
    ?assertEqual(integer, FromJsonRule),
    ?assertEqual(integer, ToJsonRule).

object_schema_generates_struct_rule_test() ->
    Schema = #{
        <<"type">> => <<"object">>,
        <<"required">> => [<<"count">>],
        <<"properties">> => #{
            <<"count">> => #{<<"type">> => <<"integer">>},
            <<"size">> => #{<<"type">> => <<"integer">>}
        }
    },
    #{from_json := FromJsonRule, to_json := ToJsonRule} = klsn_rule_generator:from_json_schema(Schema),
    ?assertEqual({struct, #{count => {required, integer}, size => {optional, integer}}}, FromJsonRule),
    ?assertEqual({struct, #{count => {required, integer}, size => {optional, integer}}}, ToJsonRule).

schema_rule_normalizes_integer_test() ->
    Schema = #{<<"type">> => <<"integer">>},
    Rule = {struct, #{type => {required, {enum, [integer]}}}},
    ?assertEqual(#{type => integer}, klsn_rule:normalize(Rule, Schema)).

json_to_term_and_back_golden_test() ->
    Schema = #{
        <<"type">> => <<"object">>,
        <<"required">> => [<<"count">>],
        <<"properties">> => #{
            <<"count">> => #{<<"type">> => <<"integer">>}
        }
    },
    #{from_json := FromJsonRule, to_json := ToJsonRule} = klsn_rule_generator:from_json_schema(Schema),
    Json = <<"{\"count\":10}">>,
    JsonMap = jsone:decode(Json),
    Term = klsn_rule:normalize(FromJsonRule, JsonMap),
    ?assertEqual(#{count => 10}, Term),
    BackTerm = klsn_rule:normalize(ToJsonRule, Term),
    ?assertEqual(#{count => 10}, BackTerm),
    ?assertEqual(Json, jsone:encode(BackTerm)).
