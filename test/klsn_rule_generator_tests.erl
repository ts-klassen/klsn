-module(klsn_rule_generator_tests).
-include_lib("eunit/include/eunit.hrl").

integer_schema_generates_integer_rule_test() ->
    Schema = #{<<"type">> => <<"integer">>},
    #{from_json := FromJsonRule, to_json := ToJsonRule} = klsn_rule_generator:from_json_schema(Schema),
    ?assertEqual(integer, FromJsonRule),
    ?assertEqual(integer, ToJsonRule).

-spec charlist_integer_type_test() -> ok.
charlist_integer_type_test() ->
    Rules = klsn_rule_generator:from_json_schema(#{type => "integer"}),
    lists:foreach(fun(Direction) ->
        Rule = maps:get(Direction, Rules),
        ?assertEqual(integer, Rule),
        ?assertEqual(7, normalize_direction(7, Rule, Direction)),
        assert_rejected(#{}, Rule, Direction)
    end, [from_json, to_json]).

-spec mixed_iolist_type_test() -> ok.
mixed_iolist_type_test() ->
    Types = [
        [<<"inte">> | "ger"]
      , [$i, <<"nte">> | "ger"]
      , [<<"inte">>, <<"ger">>]
      , [[<<"in">>, $t], <<"eger">>]
    ],
    lists:foreach(fun(Type) ->
        Rules = klsn_rule_generator:from_json_schema(#{type => Type}),
        lists:foreach(fun(Direction) ->
            Rule = maps:get(Direction, Rules),
            ?assertEqual(integer, Rule),
            ?assertEqual(7, normalize_direction(7, Rule, Direction)),
            assert_rejected(#{}, Rule, Direction)
        end, [from_json, to_json])
    end, Types).

-spec iolist_object_choice_test() -> ok.
iolist_object_choice_test() ->
    lists:foreach(fun(Choice) ->
        Schema = #{
            type => [<<"obj">> | "ect"]
          , properties => #{<<"count">> => #{type => [<<"inte">> | "ger"]}}
          , required => [<<"count">>]
          , Choice => [#{
                type => [<<"obj">>, <<"ect">>]
              , properties => #{<<"value">> => #{type => [<<"str">> | "ing"]}}
              , required => [<<"value">>]
            }]
        },
        Rules = klsn_rule_generator:from_json_schema(Schema),
        lists:foreach(fun(Direction) ->
            Rule = maps:get(Direction, Rules),
            Input = #{count => 7, value => <<"ok">>},
            ?assertEqual(Input, normalize_direction(Input, Rule, Direction)),
            assert_rejected(maps:remove(count, Input), Rule, Direction),
            assert_rejected(maps:remove(value, Input), Rule, Direction),
            assert_rejected(Input#{count => #{}}, Rule, Direction)
        end, [from_json, to_json])
    end, [anyOf, oneOf]).

-spec charlist_object_type_test() -> ok.
charlist_object_type_test() ->
    Base = #{
        type => "object"
      , properties => #{<<"count">> => #{type => "integer"}}
      , required => [<<"count">>]
    },
    Branch = #{
        type => "object"
      , properties => #{<<"count">> => #{type => "integer"}}
    },
    lists:foreach(fun(Schema) ->
        Rules = klsn_rule_generator:from_json_schema(Schema),
        lists:foreach(fun(Direction) ->
            Rule = maps:get(Direction, Rules),
            ?assertEqual(#{count => 7}, normalize_direction(#{count => 7}, Rule, Direction)),
            assert_rejected(#{}, Rule, Direction),
            assert_rejected(#{count => #{}}, Rule, Direction),
            assert_rejected(7, Rule, Direction)
        end, [from_json, to_json])
    end, [Base, Base#{anyOf => [Branch]}, Base#{oneOf => [Branch]}]).

-spec charlist_type_list_test() -> ok.
charlist_type_list_test() ->
    lists:foreach(fun(Types) ->
        Schema = #{type => Types, items => #{type => "integer"}},
        Rules = klsn_rule_generator:from_json_schema(Schema),
        lists:foreach(fun(Direction) ->
            Rule = maps:get(Direction, Rules),
            ?assertEqual([7], normalize_direction([7], Rule, Direction)),
            ?assertEqual(null, normalize_direction(null, Rule, Direction)),
            assert_rejected([#{}], Rule, Direction),
            assert_rejected(#{}, Rule, Direction)
        end, [from_json, to_json])
    end, [
        ["array", "null"]
      , [array, null]
      , [<<"array">>, <<"null">>]
      , [[<<"ar">> | "ray"], [<<"nu">>, <<"ll">>]]
    ]).

boolean_schema_true_generates_term_rule_test() ->
    #{from_json := FromJsonRule, to_json := ToJsonRule} = klsn_rule_generator:from_json_schema(true),
    ?assertEqual(term, FromJsonRule),
    ?assertEqual(term, ToJsonRule).

boolean_schema_false_generates_reject_rule_test() ->
    #{from_json := FromJsonRule, to_json := ToJsonRule} = klsn_rule_generator:from_json_schema(false),
    ?assertEqual({enum, []}, FromJsonRule),
    ?assertEqual({enum, []}, ToJsonRule).

ref_schema_generates_with_defs_rule_test() ->
    Schema = #{
        <<"$ref">> => <<"#/definitions/sample">>,
        <<"definitions">> => #{
            <<"sample">> => #{<<"type">> => <<"integer">>}
        }
    },
    #{from_json := FromJsonRule, to_json := ToJsonRule} = klsn_rule_generator:from_json_schema(Schema),
    Expected = {with_defs, {#{<<"sample">> => integer}, {ref, <<"sample">>}}},
    ?assertEqual(Expected, FromJsonRule),
    ?assertEqual(Expected, ToJsonRule),
    RecursiveSchema = #{
        <<"$ref">> => <<"#/definitions/node">>,
        <<"definitions">> => #{
            <<"node">> => #{
                <<"anyOf">> => [
                    #{<<"type">> => <<"integer">>},
                    #{
                        <<"type">> => <<"array">>,
                        <<"items">> => #{<<"$ref">> => <<"#/definitions/node">>}
                    }
                ]
            }
        }
    },
    #{from_json := RecursiveFrom, to_json := RecursiveTo} =
        klsn_rule_generator:from_json_schema(RecursiveSchema),
    RecursiveExpected = {with_defs, {#{
        <<"node">> => {any_of, [integer, {list, {ref, <<"node">>}}]}
    }, {ref, <<"node">>}}},
    ?assertEqual(RecursiveExpected, RecursiveFrom),
    ?assertEqual(RecursiveExpected, RecursiveTo).

any_of_schema_generates_any_of_rule_test() ->
    Schema = #{
        <<"anyOf">> => [
            #{<<"type">> => <<"integer">>},
            #{<<"type">> => <<"string">>}
        ]
    },
    #{from_json := FromJsonRule, to_json := ToJsonRule} = klsn_rule_generator:from_json_schema(Schema),
    ?assertEqual({any_of, [integer, binstr]}, FromJsonRule),
    ?assertEqual({any_of, [integer, binstr]}, ToJsonRule).

all_of_schema_generates_all_of_rule_test() ->
    Schema = #{
        <<"allOf">> => [
            #{<<"type">> => <<"integer">>},
            #{<<"type">> => <<"number">>}
        ]
    },
    #{from_json := FromJsonRule, to_json := ToJsonRule} = klsn_rule_generator:from_json_schema(Schema),
    ?assertEqual({all_of, [integer, number]}, FromJsonRule),
    ?assertEqual({all_of, [integer, number]}, ToJsonRule).

one_of_schema_generates_any_of_rule_test() ->
    Schema = #{
        <<"oneOf">> => [
            #{<<"type">> => <<"integer">>},
            #{<<"type">> => <<"string">>}
        ]
    },
    #{from_json := FromJsonRule, to_json := ToJsonRule} = klsn_rule_generator:from_json_schema(Schema),
    ?assertEqual({any_of, [integer, binstr]}, FromJsonRule),
    ?assertEqual({any_of, [integer, binstr]}, ToJsonRule).

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

nullable_array_schema_generates_any_of_rule_test() ->
    Schema = #{
        <<"type">> => [<<"array">>, <<"null">>],
        <<"items">> => #{<<"type">> => <<"integer">>}
    },
    #{from_json := FromJsonRule, to_json := ToJsonRule} = klsn_rule_generator:from_json_schema(Schema),
    Expected = {any_of, [{list, integer}, {exact, null}]},
    ?assertEqual(Expected, FromJsonRule),
    ?assertEqual(Expected, ToJsonRule).

null_schema_generates_null_rule_test() ->
    Schema = #{<<"type">> => <<"null">>},
    #{from_json := FromJsonRule, to_json := ToJsonRule} = klsn_rule_generator:from_json_schema(Schema),
    ?assertEqual({exact, null}, FromJsonRule),
    ?assertEqual({exact, null}, ToJsonRule).

enum_schema_generates_enum_rule_test() ->
    Schema = #{<<"enum">> => [<<"a">>, <<"b">>]},
    #{from_json := FromJsonRule, to_json := ToJsonRule} = klsn_rule_generator:from_json_schema(Schema),
    ?assertEqual({enum, [a, b]}, FromJsonRule),
    ?assertEqual({enum, [a, b]}, ToJsonRule).

const_schema_generates_exact_rule_test() ->
    Schema = #{<<"const">> => <<"ok">>},
    #{from_json := FromJsonRule, to_json := ToJsonRule} = klsn_rule_generator:from_json_schema(Schema),
    ?assertEqual({exact, <<"ok">>}, FromJsonRule),
    ?assertEqual({exact, <<"ok">>}, ToJsonRule).

-spec const_schema_with_nonstring_enum_test() -> ok.
const_schema_with_nonstring_enum_test() ->
    Schema = #{<<"const">> => <<"ok">>, <<"enum">> => [<<"ok">>, #{}]},
    Rules = klsn_rule_generator:from_json_schema(Schema),
    lists:foreach(fun(Direction) ->
        Rule = maps:get(Direction, Rules),
        ?assertEqual({exact, <<"ok">>}, Rule),
        ?assertEqual(<<"ok">>, normalize_direction(<<"ok">>, Rule, Direction)),
        assert_rejected(#{}, Rule, Direction)
    end, [from_json, to_json]).

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
    ?assertEqual(#{type => integer}, klsn_rule:normalize(Schema, Rule)).

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
    Term = klsn_rule:normalize(JsonMap, FromJsonRule),
    ?assertEqual(#{count => 10}, Term),
    BackTerm = klsn_rule:normalize(Term, ToJsonRule),
    ?assertEqual(#{count => 10}, BackTerm),
    ?assertEqual(Json, jsone:encode(BackTerm)).

-spec common_object_constraints_test_() -> [{pos_integer(), fun(() -> ok)}].
common_object_constraints_test_() ->
    [
        ?_test(check_common_object(<<"anyOf">>))
      , ?_test(check_common_object(<<"oneOf">>))
    ].

-spec check_common_object(klsn:binstr()) -> ok.
check_common_object(Choice) ->
    Schema = image_schema(Choice),
    Rules = klsn_rule_generator:from_json_schema(Schema),
    lists:foreach(fun(Direction) ->
        Rule = maps:get(Direction, Rules),
        lists:foreach(fun(Field) ->
            Base = #{type => image, Field => <<"example">>},
            lists:foreach(fun(Input) ->
                ?assertEqual(Input, normalize_direction(Input, Rule, Direction))
            end, [Base, Base#{detail => high}, Base#{detail => null}]),
            assert_rejected(maps:remove(type, Base), Rule, Direction),
            assert_rejected(Base#{type => wrong}, Rule, Direction)
        end, [url, fileId]),
        assert_rejected(#{type => image}, Rule, Direction),
        assert_rejected(42, Rule, Direction)
    end, [from_json, to_json]).

-spec image_schema(klsn:binstr()) -> klsn_rule_generator:json_schema().
image_schema(Choice) ->
    Branches = lists:map(fun(Field) ->
        #{
            <<"type">> => <<"object">>
          , <<"properties">> => #{Field => #{<<"type">> => <<"string">>}}
          , <<"required">> => [Field]
        }
    end, [<<"url">>, <<"fileId">>]),
    #{
        <<"type">> => <<"object">>
      , <<"properties">> => #{
            <<"type">> => #{<<"type">> => <<"string">>, <<"enum">> => [<<"image">>]}
          , <<"detail">> => #{
                <<"anyOf">> => [
                    #{<<"enum">> => [<<"high">>, <<"low">>]}
                  , #{<<"type">> => <<"null">>}
                ]
              , <<"default">> => null
            }
        }
      , <<"required">> => [<<"type">>]
      , Choice => Branches
    }.

-spec normalize_direction(klsn_rule:input(), klsn_rule:rule(), from_json | to_json) -> klsn_rule:output().
normalize_direction(Input, Rule, Direction) ->
    klsn_rule:normalize(direction_input(Input, Direction), Rule).

-spec direction_input(klsn_rule:input(), from_json | to_json) -> klsn_rule:input().
direction_input(Input, from_json) ->
    jsone:decode(jsone:encode(Input));
direction_input(Input, to_json) ->
    Input.

-spec assert_rejected(klsn_rule:input(), klsn_rule:rule(), from_json | to_json) -> ok.
assert_rejected(Input, Rule, Direction) ->
    ?assertMatch({reject, _}, klsn_rule:eval(direction_input(Input, Direction), Rule, #{})).

-spec overlapping_property_constraints_test() -> ok.
overlapping_property_constraints_test() ->
    %% Both required lists and both enum restrictions must survive. The common
    %% side allows a/b, the branch b/c; only b is in the intersection.
    Common = #{type => object, properties => #{<<"value">> => #{enum => [<<"a">>, <<"b">>]}}},
    Branch = #{type => object, properties => #{<<"value">> => #{enum => [<<"b">>, <<"c">>]}}},
    lists:foreach(fun({CommonSchema, BranchSchema}) ->
        Schema = CommonSchema#{required => [<<"value">>], anyOf => [BranchSchema]},
        Rules = klsn_rule_generator:from_json_schema(Schema),
        lists:foreach(fun(Direction) ->
            Rule = maps:get(Direction, Rules),
            ?assertEqual(#{value => b}, normalize_direction(#{value => b}, Rule, Direction)),
            lists:foreach(fun(Value) ->
                assert_rejected(#{value => Value}, Rule, Direction)
            end, [a, c]),
            assert_rejected(#{}, Rule, Direction)
        end, [from_json, to_json])
    end, [{Common, Branch}, {Branch, Common}]).

-spec nested_overlapping_objects_test() -> ok.
nested_overlapping_objects_test() ->
    Common = #{type => object, properties => #{<<"a">> => #{type => string}}, required => [<<"a">>]},
    Branch = #{type => object, properties => #{<<"b">> => #{type => string}}, required => [<<"b">>]},
    Schema = #{
        type => object
      , properties => #{<<"nested">> => Common}
      , required => [<<"nested">>]
      , oneOf => [#{type => object, properties => #{<<"nested">> => Branch}}]
    },
    Rules = klsn_rule_generator:from_json_schema(Schema),
    lists:foreach(fun(Direction) ->
        Rule = maps:get(Direction, Rules),
        Input = #{nested => #{a => <<"A">>, b => <<"B">>}},
        ?assertEqual(Input, normalize_direction(Input, Rule, Direction)),
        lists:foreach(fun(Value) ->
            assert_rejected(#{nested => Value}, Rule, Direction)
        end, [#{a => <<"A">>}, #{b => <<"B">>}])
    end, [from_json, to_json]).

-spec unsupported_choice_intersections_test() -> ok.
unsupported_choice_intersections_test() ->
    Schema = image_schema(<<"anyOf">>),
    [Branch|_] = maps:get(<<"anyOf">>, Schema),
    Unsupported = [
        Schema#{<<"additionalProperties">> => false}
      , Schema#{<<"additionalProperties">> => #{<<"type">> => <<"string">>}}
      , Schema#{<<"anyOf">> => [Branch#{<<"additionalProperties">> => false}]}
      , Schema#{<<"anyOf">> => [Branch#{<<"default">> => #{}}]}
      , Schema#{<<"anyOf">> => [#{<<"$ref">> => <<"#/definitions/Branch">>}]}
      , Schema#{<<"allOf">> => [true]}
      , Schema#{<<"maxProperties">> => 2}
      , Schema#{<<"anyOf">> => [#{<<"type">> => <<"string">>}]}
      , #{type => string, anyOf => [#{enum => [<<"a">>]}]}
      , #{
            type => object
          , properties => #{<<"v">> => #{type => string}}
          , anyOf => [#{type => object, properties => #{<<"v">> => #{pattern => <<"a">>}}}]
        }
    ],
    lists:foreach(fun(UnsupportedSchema) ->
        ?assertException(
            error
          , {klsn_rule_generator, unsupported_intersection, _}
          , klsn_rule_generator:from_json_schema(UnsupportedSchema)
        )
    end, Unsupported).

-spec choice_without_common_constraints_behavior_test() -> ok.
choice_without_common_constraints_behavior_test() ->
    lists:foreach(fun(Choice) ->
        Rules = klsn_rule_generator:from_json_schema(#{Choice => [#{type => integer}, #{type => string}]}),
        lists:foreach(fun(Direction) ->
            Rule = maps:get(Direction, Rules),
            ?assertEqual(7, normalize_direction(7, Rule, Direction)),
            ?assertEqual(<<"hello">>, normalize_direction(<<"hello">>, Rule, Direction)),
            assert_rejected(#{}, Rule, Direction)
        end, [from_json, to_json])
    end, [anyOf, oneOf]),
    %% Preserve the historical oneOf-as-any_of policy for overlapping branches.
    #{to_json := Rule} = klsn_rule_generator:from_json_schema(#{oneOf => [#{type => integer}, #{type => number}]}),
    ?assertEqual(7, klsn_rule:normalize(7, Rule)).

-spec choice_default_and_additional_properties_test() -> ok.
choice_default_and_additional_properties_test() ->
    Base = image_schema(<<"anyOf">>),
    Default = #{<<"type">> => <<"image">>, <<"fileId">> => <<"fallback">>},
    Schema = Base#{<<"default">> => Default, <<"additionalProperties">> => true},
    Rules = klsn_rule_generator:from_json_schema(Schema),
    lists:foreach(fun(Direction) ->
        Rule = maps:get(Direction, Rules),
        %% Defaults are returned verbatim by the existing default rule.
        ?assertEqual(Default, normalize_direction(#{}, Rule, Direction)),
        %% Keep the established struct projection convention for extra keys.
        ?assertEqual(
            #{type => image, url => <<"url">>}
          , normalize_direction(#{type => image, url => <<"url">>, extra => 1}, Rule, Direction)
        )
    end, [from_json, to_json]).

-spec property_intersection_edges_test() -> ok.
property_intersection_edges_test() ->
    String = #{type => string},
    Enum = #{enum => [<<"b">>]},
    Accepted = [
        {String, Enum}
      , {Enum, String}
      , {true, Enum}
      , {Enum, true}
      , {#{enum => [a, b]}, Enum}
      , {#{enum => ["a", "b"]}, Enum}
      , {Enum, #{enum => ["a", "b"]}}
    ],
    lists:foreach(fun({Common, Branch}) ->
        Schema = overlap_schema(Common, Branch),
        Rules = klsn_rule_generator:from_json_schema(Schema),
        lists:foreach(fun(Direction) ->
            Rule = maps:get(Direction, Rules),
            ?assertEqual(#{value => b}, normalize_direction(#{value => b}, Rule, Direction)),
            assert_rejected(#{value => a}, Rule, Direction)
        end, [from_json, to_json])
    end, Accepted),
    lists:foreach(fun({Common, Branch}) ->
        Rules = klsn_rule_generator:from_json_schema(overlap_schema(Common, Branch)),
        lists:foreach(fun(Direction) ->
            lists:foreach(fun(Value) ->
                assert_rejected(#{value => Value}, maps:get(Direction, Rules), Direction)
            end, [a, b])
        end, [from_json, to_json])
    end, [{#{enum => [<<"a">>]}, Enum}, {false, Enum}, {Enum, false}]),
    Unsupported = [
        {String#{default => <<"a">>}, String#{default => <<"b">>}}
      , {String#{default => <<"a">>}, Enum}
      , {#{'$ref' => <<"#/definitions/A">>}, String}
      , {#{type => integer}, Enum}
    ],
    lists:foreach(fun({Common, Branch}) ->
        ?assertException(
            error
          , {klsn_rule_generator, unsupported_intersection, _}
          , klsn_rule_generator:from_json_schema(overlap_schema(Common, Branch))
        )
    end, Unsupported).

-spec overlap_schema(klsn_rule_generator:json_schema(), klsn_rule_generator:json_schema()) ->
    klsn_rule_generator:json_schema().
overlap_schema(Common, Branch) ->
    #{
        type => object
      , properties => #{<<"value">> => Common}
      , required => [<<"value">>]
      , anyOf => [#{type => object, properties => #{<<"value">> => Branch}}]
    }.

-spec choice_required_across_property_sets_test() -> ok.
choice_required_across_property_sets_test() ->
    Schema = #{
        properties => #{<<"a">> => #{type => string}}
      , required => [<<"b">>]
      , oneOf => [#{
            type => object
          , properties => #{<<"b">> => #{type => string}}
          , required => [<<"a">>]
        }]
    },
    Rules = klsn_rule_generator:from_json_schema(Schema),
    lists:foreach(fun(Direction) ->
        Rule = maps:get(Direction, Rules),
        Input = #{a => <<"A">>, b => <<"B">>},
        ?assertEqual(Input, normalize_direction(Input, Rule, Direction)),
        lists:foreach(fun(Key) ->
            assert_rejected(maps:remove(Key, Input), Rule, Direction)
        end, [a, b])
    end, [from_json, to_json]).

-spec choice_property_key_normalization_test() -> ok.
choice_property_key_normalization_test() ->
    %% Noncanonical key forms are intentional boundary inputs. Each pair
    %% denotes the same property under the existing binstr normalization.
    Keys = [
        {v, "v", <<"v">>}
      , {"v", v, <<"v">>}
      , {<<"v">>, [<<"v">>], <<"v">>}
      , {1, <<"1">>, <<"1">>}
      , {1.0, <<"1.00000000000000000000e+00">>, <<"1.00000000000000000000e+00">>}
    ],
    lists:foreach(fun(Choice) ->
        lists:foreach(fun({CommonKey, BranchKey, JsonKey}) ->
            Schema = #{
                type => object
              , properties => #{CommonKey => #{type => integer}}
              , required => [CommonKey]
              , Choice => [#{
                    type => object
                  , properties => #{BranchKey => #{type => integer}}
                  , required => [BranchKey]
                }]
            },
            Rules = klsn_rule_generator:from_json_schema(Schema),
            lists:foreach(fun(Direction) ->
                Rule = maps:get(Direction, Rules),
                Input = #{JsonKey => 7},
                Output = normalize_direction(Input, Rule, Direction),
                ?assertEqual(Input, jsone:decode(jsone:encode(Output))),
                assert_rejected(42, Rule, Direction),
                assert_rejected(#{}, Rule, Direction),
                assert_rejected(#{JsonKey => <<"invalid">>}, Rule, Direction)
            end, [from_json, to_json])
        end, Keys)
    end, [anyOf, oneOf]).

-spec conflicting_property_key_forms_test() -> ok.
conflicting_property_key_forms_test() ->
    Schema = #{
        type => object
      , properties => #{
            v => #{type => integer}
          , "v" => #{type => string}
        }
    },
    ?assertException(
        error
      , {klsn_rule_generator, conflicting_schema_keys, _}
      , klsn_rule_generator:from_json_schema(Schema)
    ).

-spec choice_id_metadata_test() -> ok.
choice_id_metadata_test() ->
    lists:foreach(fun(Choice) ->
        lists:foreach(fun(IdKey) ->
            Schema = #{
                IdKey => <<"https://example.org/test.json">>
              , Choice => [#{type => integer}, #{type => string}]
            },
            Rules = klsn_rule_generator:from_json_schema(Schema),
            lists:foreach(fun(Direction) ->
                Rule = maps:get(Direction, Rules),
                ?assertEqual(7, normalize_direction(7, Rule, Direction)),
                ?assertEqual(<<"hello">>, normalize_direction(<<"hello">>, Rule, Direction)),
                assert_rejected(#{}, Rule, Direction)
            end, [from_json, to_json])
        end, ['$id', <<"$id">>])
    end, [anyOf, oneOf]).
