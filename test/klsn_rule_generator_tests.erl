-module(klsn_rule_generator_tests).
-include_lib("eunit/include/eunit.hrl").

integer_schema_generates_integer_rule_test() ->
    SchemaJson = <<"{\"type\":\"integer\"}">>,
    Schema = jsone:decode(SchemaJson),
    #{from_json := FromJsonRule, to_json := ToJsonRule} = klsn_rule_generator:from_json_schema(Schema),
    ?assertEqual(integer, FromJsonRule),
    ?assertEqual(integer, ToJsonRule).

schema_rule_normalizes_integer_test() ->
    SchemaJson = <<"{\"type\":\"integer\"}">>,
    Schema = jsone:decode(SchemaJson),
    Rule = {struct, #{type => {required, {enum, [integer]}}}},
    ?assertEqual(#{type => integer}, klsn_rule:normalize(Rule, Schema)).
