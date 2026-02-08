-module(klsn_rule_generator_tests).
-include_lib("eunit/include/eunit.hrl").

integer_schema_generates_integer_rule_test() ->
    Schema = #{<<"type">> => <<"integer">>},
    #{from_json := FromJsonRule, to_json := ToJsonRule} = klsn_rule_generator:from_json_schema(Schema),
    ?assertEqual(integer, FromJsonRule),
    ?assertEqual(integer, ToJsonRule).

boolean_schema_true_generates_term_rule_test() ->
    #{from_json := FromJsonRule, to_json := ToJsonRule} = klsn_rule_generator:from_json_schema(true),
    ?assertEqual(term, FromJsonRule),
    ?assertEqual(term, ToJsonRule).

boolean_schema_false_generates_reject_rule_test() ->
    #{from_json := FromJsonRule, to_json := ToJsonRule} = klsn_rule_generator:from_json_schema(false),
    ?assertEqual({enum, []}, FromJsonRule),
    ?assertEqual({enum, []}, ToJsonRule).

string_schema_generates_binstr_rule_test() ->
    Schema = #{<<"type">> => <<"string">>},
    #{from_json := FromJsonRule, to_json := ToJsonRule} = klsn_rule_generator:from_json_schema(Schema),
    ?assertEqual(binstr, FromJsonRule),
    ?assertEqual(binstr, ToJsonRule).

boolean_schema_generates_boolean_rule_test() ->
    Schema = #{<<"type">> => <<"boolean">>},
    #{from_json := FromJsonRule, to_json := ToJsonRule} = klsn_rule_generator:from_json_schema(Schema),
    ?assertEqual(boolean, FromJsonRule),
    ?assertEqual(boolean, ToJsonRule).

number_schema_generates_number_rule_test() ->
    Schema = #{<<"type">> => <<"number">>},
    #{from_json := FromJsonRule, to_json := ToJsonRule} = klsn_rule_generator:from_json_schema(Schema),
    ?assertEqual(number, FromJsonRule),
    ?assertEqual(number, ToJsonRule).

float_schema_generates_float_rule_test() ->
    Schema = #{<<"type">> => <<"float">>},
    #{from_json := FromJsonRule, to_json := ToJsonRule} = klsn_rule_generator:from_json_schema(Schema),
    ?assertEqual(float, FromJsonRule),
    ?assertEqual(float, ToJsonRule).

array_schema_generates_list_rule_test() ->
    Schema = #{<<"type">> => <<"array">>, <<"items">> => #{<<"type">> => <<"integer">>}},
    #{from_json := FromJsonRule, to_json := ToJsonRule} = klsn_rule_generator:from_json_schema(Schema),
    ?assertEqual({list, integer}, FromJsonRule),
    ?assertEqual({list, integer}, ToJsonRule).

array_schema_defaults_to_term_items_test() ->
    Schema = #{<<"type">> => <<"array">>},
    #{from_json := FromJsonRule, to_json := ToJsonRule} = klsn_rule_generator:from_json_schema(Schema),
    ?assertEqual({list, term}, FromJsonRule),
    ?assertEqual({list, term}, ToJsonRule).

null_schema_generates_null_rule_test() ->
    Schema = #{<<"type">> => <<"null">>},
    #{from_json := FromJsonRule, to_json := ToJsonRule} = klsn_rule_generator:from_json_schema(Schema),
    ?assertEqual({exact, null}, FromJsonRule),
    ?assertEqual({exact, null}, ToJsonRule).

enum_schema_generates_enum_rule_test() ->
    Schema = #{<<"enum">> => [<<"a">>, <<"b">>]},
    #{from_json := FromJsonRule, to_json := ToJsonRule} = klsn_rule_generator:from_json_schema(Schema),
    ?assertEqual({enum, [<<"a">>, <<"b">>]}, FromJsonRule),
    ?assertEqual({enum, [<<"a">>, <<"b">>]}, ToJsonRule).

const_schema_generates_exact_rule_test() ->
    Schema = #{<<"const">> => <<"ok">>},
    #{from_json := FromJsonRule, to_json := ToJsonRule} = klsn_rule_generator:from_json_schema(Schema),
    ?assertEqual({exact, <<"ok">>}, FromJsonRule),
    ?assertEqual({exact, <<"ok">>}, ToJsonRule).

default_schema_wraps_rule_test() ->
    Schema = #{<<"type">> => <<"boolean">>, <<"default">> => false},
    #{from_json := FromJsonRule, to_json := ToJsonRule} = klsn_rule_generator:from_json_schema(Schema),
    ?assertEqual({default, {false, boolean}}, FromJsonRule),
    ?assertEqual({default, {false, boolean}}, ToJsonRule).

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
