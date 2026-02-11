-module(klsn_rule_generator).
-include_lib("klsn/include/klsn_rule_annotation.hrl").

-export([
        from_json_schema/1
      , from_json_schema/2
    ]).

-klsn_rule_alias([
        {json_schema, {any_of, [
            {alias, {?MODULE, json_schema_true}},
            {alias, {?MODULE, json_schema_false}},
            {alias, {?MODULE, json_schema_ref}},
            {alias, {?MODULE, json_schema_any_of}},
            {alias, {?MODULE, json_schema_all_of}},
            {alias, {?MODULE, json_schema_one_of}},
            {alias, {?MODULE, json_schema_const}},
            {alias, {?MODULE, json_schema_enum}},
            {alias, {?MODULE, json_schema_type_list}},
            {alias, {?MODULE, json_schema_integer}},
            {alias, {?MODULE, json_schema_string}},
            {alias, {?MODULE, json_schema_boolean}},
            {alias, {?MODULE, json_schema_number}},
            {alias, {?MODULE, json_schema_float}},
            {alias, {?MODULE, json_schema_null}},
            {alias, {?MODULE, json_schema_array}},
            {alias, {?MODULE, json_schema_object}},
            {alias, {?MODULE, json_schema_annotations}}
        ]}}
      , {json_schema_definitions, {map, {binstr, {alias, {?MODULE, json_schema}}}}}
      , {json_schema_ref, {struct, #{
            '$ref' => {required, binstr},
            definitions => {optional, {alias, {?MODULE, json_schema_definitions}}},
            default => {optional, {alias, {?MODULE, json_value}}}
        }}}
      , {json_schema_true, {exact, true}}
      , {json_schema_false, {exact, false}}
      , {json_schema_any_of, {struct, #{
            anyOf => {required, {list, {alias, {?MODULE, json_schema}}}},
            definitions => {optional, {alias, {?MODULE, json_schema_definitions}}},
            default => {optional, {alias, {?MODULE, json_value}}}
        }}}
      , {json_schema_all_of, {struct, #{
            allOf => {required, {list, {alias, {?MODULE, json_schema}}}},
            definitions => {optional, {alias, {?MODULE, json_schema_definitions}}},
            default => {optional, {alias, {?MODULE, json_value}}}
        }}}
      , {json_schema_one_of, {struct, #{
            oneOf => {required, {list, {alias, {?MODULE, json_schema}}}},
            definitions => {optional, {alias, {?MODULE, json_schema_definitions}}},
            default => {optional, {alias, {?MODULE, json_value}}}
        }}}
      , {json_schema_type_list, {struct, #{
            type => {required, {list, {enum, [integer, string, boolean, number, float, null, array, object]}}},
            items => {optional, {alias, {?MODULE, json_schema}}},
            properties => {optional, {map, {binstr, {alias, {?MODULE, json_schema}}}}},
            required => {optional, {list, binstr}},
            definitions => {optional, {alias, {?MODULE, json_schema_definitions}}},
            default => {optional, {alias, {?MODULE, json_value}}}
        }}}
      , {json_value, {any_of, [
            {exact, null},
            boolean,
            number,
            binstr,
            {list, {alias, {?MODULE, json_value}}},
            {map, {binstr, {alias, {?MODULE, json_value}}}}
        ]}}
      , {json_schema_integer, {struct, #{
            type => {required, {enum, [integer]}},
            definitions => {optional, {alias, {?MODULE, json_schema_definitions}}},
            default => {optional, {alias, {?MODULE, json_value}}}
        }}}
      , {json_schema_string, {struct, #{
            type => {required, {enum, [string]}},
            definitions => {optional, {alias, {?MODULE, json_schema_definitions}}},
            default => {optional, {alias, {?MODULE, json_value}}}
        }}}
      , {json_schema_boolean, {struct, #{
            type => {required, {enum, [boolean]}},
            definitions => {optional, {alias, {?MODULE, json_schema_definitions}}},
            default => {optional, {alias, {?MODULE, json_value}}}
        }}}
      , {json_schema_number, {struct, #{
            type => {required, {enum, [number]}},
            definitions => {optional, {alias, {?MODULE, json_schema_definitions}}},
            default => {optional, {alias, {?MODULE, json_value}}}
        }}}
      , {json_schema_float, {struct, #{
            type => {required, {enum, [float]}},
            definitions => {optional, {alias, {?MODULE, json_schema_definitions}}},
            default => {optional, {alias, {?MODULE, json_value}}}
        }}}
      , {json_schema_null, {struct, #{
            type => {required, {enum, [null]}},
            definitions => {optional, {alias, {?MODULE, json_schema_definitions}}},
            default => {optional, {alias, {?MODULE, json_value}}}
        }}}
      , {json_schema_array, {struct, #{
            type => {required, {enum, [array]}},
            items => {optional, {alias, {?MODULE, json_schema}}},
            definitions => {optional, {alias, {?MODULE, json_schema_definitions}}},
            default => {optional, {alias, {?MODULE, json_value}}}
        }}}
      , {json_schema_const, {struct, #{
            const => {required, {any_of, [{exact, null}, boolean, number, binstr]}},
            definitions => {optional, {alias, {?MODULE, json_schema_definitions}}},
            default => {optional, {alias, {?MODULE, json_value}}}
        }}}
      , {json_schema_enum, {struct, #{
            enum => {required, {list, binstr}},
            definitions => {optional, {alias, {?MODULE, json_schema_definitions}}},
            default => {optional, {alias, {?MODULE, json_value}}}
        }}}
      , {json_schema_object, {struct, #{
            type => {required, {enum, [object]}}
          , properties => {optional, {map, {binstr, {alias, {?MODULE, json_schema}}}}}
          , required => {optional, {list, binstr}}
          , additionalProperties => {optional, {any_of, [boolean, {alias, {?MODULE, json_schema}}]}}
          , definitions => {optional, {alias, {?MODULE, json_schema_definitions}}}
          , default => {optional, {alias, {?MODULE, json_value}}}
        }}}
      , {json_schema_annotations, {struct, #{
            description => {optional, binstr},
            title => {optional, binstr},
            default => {optional, {alias, {?MODULE, json_value}}},
            examples => {optional, {list, {alias, {?MODULE, json_value}}}},
            example => {optional, {alias, {?MODULE, json_value}}},
            '$comment' => {optional, binstr},
            deprecated => {optional, boolean},
            readOnly => {optional, boolean},
            writeOnly => {optional, boolean},
            format => {optional, binstr}
        }}}
    ]).
-type json_schema() :: klsn_rule:alias(json_schema).
-type opts() :: #{}.

-klsn_rule_alias([
        {opts_rule, {struct, #{}}}
      , {json_schema_rules, {struct, #{
            from_json => {required, term}
          , to_json => {required, term}
        }}}
    ]).
-type json_schema_rules() :: #{
        from_json => klsn_rule:rule()
      , to_json => klsn_rule:rule()
    }.

%% @doc
%% Do not accept JSON Schema from user input; this function uses binary_to_atom/2
%% when converting property names, which can exhaust the atom table.
%% Not for production use: generating rules at runtime is for development only.
%% Generate rules ahead of time and include them in releases.
-spec from_json_schema(json_schema()) -> #{from_json := klsn_rule:rule(), to_json := klsn_rule:rule()}.
from_json_schema(Schema) ->
    from_json_schema(Schema, #{}).

-klsn_input_rule([{alias, {?MODULE, json_schema}}, {alias, {?MODULE, opts_rule}}]).
-klsn_output_rule({alias, {?MODULE, json_schema_rules}}).
-spec from_json_schema(json_schema(), opts()) ->
    #{from_json := klsn_rule:rule(), to_json := klsn_rule:rule()}.
from_json_schema(Schema, Opts) ->
    Rules = from_json_schema_base_(Schema, Opts),
    case Schema of
        Map when is_map(Map) ->
            case klsn_map:lookup([definitions], Map) of
                none ->
                    Rules;
                {value, Defs} when is_map(Defs) ->
                    {FromDefs, ToDefs} = lists:foldl(fun({Name, DefSchema}, {FromAcc, ToAcc}) ->
                        #{from_json := FromRule, to_json := ToRule} = from_json_schema(DefSchema, Opts),
                        {maps:put(Name, FromRule, FromAcc), maps:put(Name, ToRule, ToAcc)}
                    end, {#{}, #{}}, maps:to_list(Defs)),
                    #{from_json := FromRule, to_json := ToRule} = Rules,
                    #{from_json => {with_defs, {FromDefs, FromRule}},
                      to_json => {with_defs, {ToDefs, ToRule}}};
                {value, _} ->
                    error({klsn_rule_generator, unsupported_schema, Schema})
            end;
        _ ->
            Rules
    end.

from_json_schema_base_(true, _Opts) ->
    json_rules_from_rule_(term);
from_json_schema_base_(false, _Opts) ->
    json_rules_from_rule_({enum, []});
from_json_schema_base_(#{'$ref' := Ref}=Schema, _Opts) ->
    RefName = case Ref of
        <<"#/definitions/", Name/binary>> when Name =/= <<>> ->
            Name;
        _ ->
            error({klsn_rule_generator, unsupported_ref, Ref})
    end,
    json_rules_from_rule_(with_default_(Schema, {ref, RefName}));
from_json_schema_base_(#{anyOf := Schemas}=Schema, Opts) ->
    {FromRules, ToRules} = json_rule_list_(Schemas, Opts),
    #{from_json => with_default_(Schema, {any_of, FromRules}),
      to_json => with_default_(Schema, {any_of, ToRules})};
from_json_schema_base_(#{allOf := Schemas}=Schema, Opts) ->
    {FromRules, ToRules} = json_rule_list_(Schemas, Opts),
    #{from_json => with_default_(Schema, {all_of, FromRules}),
      to_json => with_default_(Schema, {all_of, ToRules})};
from_json_schema_base_(#{oneOf := Schemas}=Schema, Opts) ->
    {FromRules, ToRules} = json_rule_list_(Schemas, Opts),
    #{from_json => with_default_(Schema, {any_of, FromRules}),
      to_json => with_default_(Schema, {any_of, ToRules})};
from_json_schema_base_(#{const := Const}=Schema, _Opts) ->
    json_rules_from_rule_(with_default_(Schema, {exact, Const}));
from_json_schema_base_(#{enum := Enum}=Schema, _Opts) ->
    json_rules_from_rule_(with_default_(Schema, {enum, Enum}));
from_json_schema_base_(#{type := Types}=Schema, Opts) when is_list(Types) ->
    SchemaNoDefault = maps:remove(default, Schema),
    {FromRev, ToRev} = lists:foldl(fun(Type, {FromAcc, ToAcc}) ->
        Schema1 = maps:put(type, Type, SchemaNoDefault),
        #{from_json := FromRule, to_json := ToRule} = from_json_schema_base_(Schema1, Opts),
        {[FromRule|FromAcc], [ToRule|ToAcc]}
    end, {[], []}, Types),
    FromRules = lists:reverse(FromRev),
    ToRules = lists:reverse(ToRev),
    #{from_json => with_default_(Schema, {any_of, FromRules}),
      to_json => with_default_(Schema, {any_of, ToRules})};
from_json_schema_base_(#{type := integer}=Schema, _Opts) ->
    json_rules_from_rule_(with_default_(Schema, integer));
from_json_schema_base_(#{type := string}=Schema, _Opts) ->
    json_rules_from_rule_(with_default_(Schema, binstr));
from_json_schema_base_(#{type := boolean}=Schema, _Opts) ->
    json_rules_from_rule_(with_default_(Schema, boolean));
from_json_schema_base_(#{type := number}=Schema, _Opts) ->
    json_rules_from_rule_(with_default_(Schema, number));
from_json_schema_base_(#{type := float}=Schema, _Opts) ->
    json_rules_from_rule_(with_default_(Schema, float));
from_json_schema_base_(#{type := null}=Schema, _Opts) ->
    json_rules_from_rule_(with_default_(Schema, {exact, null}));
from_json_schema_base_(#{type := array}=Schema, Opts) ->
    case klsn_map:lookup([items], Schema) of
        {value, Items} ->
            #{from_json := FromItem, to_json := ToItem} = from_json_schema(Items, Opts),
            #{from_json => with_default_(Schema, {list, FromItem}),
              to_json => with_default_(Schema, {list, ToItem})};
        none ->
            json_rules_from_rule_(with_default_(Schema, {list, term}))
    end;
from_json_schema_base_(#{type := object}=Schema, Opts) ->
    Properties = maps:get(properties, Schema, #{}),
    Required = maps:get(required, Schema, []),
    case Properties =:= #{} of
        true ->
            case Required of
                [] ->
                    case maps:get(additionalProperties, Schema, none) of
                        none ->
                            json_rules_from_rule_(with_default_(Schema, {map, {binstr, term}}));
                        true ->
                            json_rules_from_rule_(with_default_(Schema, {map, {binstr, term}}));
                        false ->
                            json_rules_from_rule_(with_default_(Schema, {struct, #{}}));
                        AddSchema when is_map(AddSchema) ->
                            #{from_json := FromItem, to_json := ToItem} = from_json_schema(AddSchema, Opts),
                            #{from_json => with_default_(Schema, {map, {binstr, FromItem}}),
                              to_json => with_default_(Schema, {map, {binstr, ToItem}})};
                        _ ->
                            error({klsn_rule_generator, unsupported_schema, Schema})
                    end;
                _ ->
                    error({klsn_rule_generator, unsupported_schema, Schema})
            end;
        false ->
            PropList = maps:to_list(Properties),
            MissingRequired = lists:filter(fun(Key) ->
                not maps:is_key(Key, Properties)
            end, Required),
            case MissingRequired of
                [] ->
                    {FromMap, ToMap} = lists:foldl(fun({PropName, PropSchema}, {FromAcc, ToAcc}) ->
                        #{from_json := FromJson, to_json := ToJson} = from_json_schema(PropSchema, Opts),
                        ReqOpt = case lists:member(PropName, Required) of
                            true -> required;
                            false -> optional
                        end,
                        Field = binary_to_atom(PropName, utf8),
                        {maps:put(Field, {ReqOpt, FromJson}, FromAcc),
                         maps:put(Field, {ReqOpt, ToJson}, ToAcc)}
                    end, {#{}, #{}}, PropList),
                    #{from_json => with_default_(Schema, {struct, FromMap}),
                      to_json => with_default_(Schema, {struct, ToMap})};
                _ ->
                    error({klsn_rule_generator, unsupported_schema, Schema})
            end
    end;
from_json_schema_base_(Schema, _Opts) when is_map(Schema) ->
    case annotation_only_schema_(Schema) of
        true ->
            json_rules_from_rule_(with_default_(Schema, term));
        false ->
            error({klsn_rule_generator, unsupported_schema, Schema})
    end;
from_json_schema_base_(Schema, _Opts) ->
    error({klsn_rule_generator, unsupported_schema, Schema}).

json_rules_from_rule_(Rule) ->
    #{from_json => Rule, to_json => Rule}.

json_rule_list_(Schemas, Opts) ->
    {FromRev, ToRev} = lists:foldl(fun(ItemSchema, {FromAcc, ToAcc}) ->
        #{from_json := FromRule, to_json := ToRule} = from_json_schema(ItemSchema, Opts),
        {[FromRule|FromAcc], [ToRule|ToAcc]}
    end, {[], []}, Schemas),
    {lists:reverse(FromRev), lists:reverse(ToRev)}.

with_default_(Schema, Rule) ->
    case klsn_map:lookup([default], Schema) of
        {value, Default} ->
            {default, {Default, Rule}};
        none ->
            Rule
    end.

annotation_only_schema_(Schema) ->
    maps:fold(fun(Key, _Value, Acc) ->
        Acc andalso annotation_key_(Key)
    end, true, Schema).

annotation_key_(Key) ->
    case Key of
        description -> true;
        title -> true;
        default -> true;
        examples -> true;
        example -> true;
        '$comment' -> true;
        deprecated -> true;
        readOnly -> true;
        writeOnly -> true;
        format -> true;
        <<"description">> -> true;
        <<"title">> -> true;
        <<"default">> -> true;
        <<"examples">> -> true;
        <<"example">> -> true;
        <<"$comment">> -> true;
        <<"deprecated">> -> true;
        <<"readOnly">> -> true;
        <<"writeOnly">> -> true;
        <<"format">> -> true;
        _ -> false
    end.
