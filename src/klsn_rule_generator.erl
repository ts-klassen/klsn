-module(klsn_rule_generator).
-include_lib("klsn/include/klsn_rule_annotation.hrl").

-export([
        from_json_schema/1
      , from_json_schema/2
    ]).

-export_type([
        json_schema/0
      , opts/0
      , json_schema_rules/0
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

-type schema_key_input() :: atom() | integer() | float() | iolist() | klsn:binstr().

%% @doc
%% Do not accept JSON Schema from user input; this function uses binary_to_atom/2
%% when converting property names and enum values, which can exhaust the atom table.
%% Not for production use: generating rules at runtime is for development only.
%% Generate rules ahead of time and include them in releases.
%%
%% For object schemas, both generated rules retain common fields and enforce
%% common type, properties, and required constraints alongside nested anyOf
%% or oneOf alternatives.
%% Unsupported combinations raise an error with reason
%% {klsn_rule_generator, unsupported_intersection, Details}.
%% oneOf accepts values matching at least one alternative; it does not require
%% exactly one match.
-spec from_json_schema(json_schema()) -> #{from_json := klsn_rule:rule(), to_json := klsn_rule:rule()}.
from_json_schema(Schema) ->
    from_json_schema(Schema, #{}).

-spec from_json_schema(json_schema(), opts()) ->
    #{from_json := klsn_rule:rule(), to_json := klsn_rule:rule()}.
from_json_schema(Schema, Opts) ->
    from_json_schema_normalized_(prepare_schema_(Schema), Opts).

-klsn_input_rule([{alias, {?MODULE, json_schema}}, {alias, {?MODULE, opts_rule}}]).
-klsn_output_rule({alias, {?MODULE, json_schema_rules}}).
-spec from_json_schema_normalized_(json_schema(), opts()) -> json_schema_rules().
from_json_schema_normalized_(Schema, Opts) ->
    Rules = from_json_schema_base_(Schema, Opts),
    case Schema of
        Map when is_map(Map) ->
            case klsn_map:lookup([definitions], Map) of
                none ->
                    Rules;
                {value, Defs} when is_map(Defs) ->
                    {FromDefs, ToDefs} = lists:foldl(fun({Name, DefSchema}, {FromAcc, ToAcc}) ->
                        #{from_json := FromRule, to_json := ToRule} = from_json_schema_normalized_(DefSchema, Opts),
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

-spec from_json_schema_base_(json_schema(), opts()) -> json_schema_rules().
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
    EnumAtoms = [binary_to_atom(Value, utf8) || Value <- Enum],
    json_rules_from_rule_(with_default_(Schema, {enum, EnumAtoms}));
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
            #{from_json := FromItem, to_json := ToItem} = from_json_schema_normalized_(Items, Opts),
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
                            #{from_json := FromItem, to_json := ToItem} = from_json_schema_normalized_(AddSchema, Opts),
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
                        #{from_json := FromJson, to_json := ToJson} = from_json_schema_normalized_(PropSchema, Opts),
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

-spec json_rule_list_([json_schema()], opts()) -> {[klsn_rule:rule()], [klsn_rule:rule()]}.
json_rule_list_(Schemas, Opts) ->
    {FromRev, ToRev} = lists:foldl(fun(ItemSchema, {FromAcc, ToAcc}) ->
        #{from_json := FromRule, to_json := ToRule} = from_json_schema_normalized_(ItemSchema, Opts),
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

%% Expand before the schema alias normalizes structs: otherwise it discards
%% siblings of anyOf/oneOf. Only schema positions are traversed, never defaults
%% or examples (which are arbitrary JSON values).
-spec prepare_schema_(json_schema()) -> json_schema().
prepare_schema_(Schema) when is_map(Schema) ->
    Canonical = schema_keys_(Schema),
    Children = maps:map(fun
        (Key, Value) when Key =:= <<"properties">>; Key =:= <<"definitions">> ->
            maps:map(fun(_, Child) ->
                prepare_schema_(Child)
            end, schema_keys_(Value));
        (Key, Value) when Key =:= <<"anyOf">>; Key =:= <<"oneOf">>; Key =:= <<"allOf">> ->
            lists:map(fun prepare_schema_/1, Value);
        (Key, Value) when Key =:= <<"items">>; Key =:= <<"additionalProperties">> ->
            prepare_schema_(Value);
        (<<"type">>, Value) ->
            %% Preserve the schema rules' precedence between a list of types
            %% and an iolist naming one type, including mixed iolists.
            TypeSchema = klsn_rule:normalize(#{type => Value}, {alias, {?MODULE, json_schema}}),
            case klsn_map:lookup([type], TypeSchema) of
                {value, Types} when is_list(Types) ->
                    lists:map(fun schema_key_/1, Types);
                {value, Type} ->
                    schema_key_(Type);
                none ->
                    Value
            end;
        (<<"required">>, Value) ->
            lists:map(fun schema_key_/1, Value);
        (_, Value) ->
            Value
    end, Canonical),
    Choices = lists:filter(fun(Key) ->
        maps:is_key(Key, Children)
    end, [<<"anyOf">>, <<"oneOf">>]),
    case Choices of
        [] ->
            Children;
        [Choice] ->
            Siblings = maps:remove(Choice, Children),
            Common = maps:filter(fun(Key, _) ->
                not choice_metadata_(Key)
            end, Siblings),
            case map_size(Common) of
                0 ->
                    Children;
                _ ->
                    ensure_mergeable_object_(Common),
                    %% Combine property constraints before struct normalization
                    %% can discard fields declared only on the other side.
                    Branches = lists:map(fun(Branch) ->
                        intersect_object_(Common, Branch)
                    end, maps:get(Choice, Children)),
                    (maps:without(maps:keys(Common), Children))#{Choice := Branches}
            end;
        _ ->
            error({klsn_rule_generator, unsupported_intersection, Children})
    end;
prepare_schema_(Schema) ->
    Schema.

-spec schema_keys_(maps:map(schema_key_input(), Value)) -> maps:map(klsn:binstr(), Value).
schema_keys_(Map) ->
    Pairs = lists:map(fun({Key, Value}) ->
        {schema_key_(Key), Value}
    end, maps:to_list(Map)),
    Result = maps:from_list(Pairs),
    case map_size(Result) =:= map_size(Map) of
        true ->
            Result;
        false ->
            error({klsn_rule_generator, conflicting_schema_keys, Map})
    end.

-spec schema_key_(schema_key_input()) -> klsn:binstr().
schema_key_(Key) ->
    klsn_rule:normalize(Key, binstr).

-spec choice_metadata_(klsn:binstr()) -> boolean().
choice_metadata_(<<"definitions">>) ->
    true;
choice_metadata_(<<"$schema">>) ->
    true;
choice_metadata_(<<"$id">>) ->
    true;
choice_metadata_(Key) ->
    annotation_key_(Key).

%% Prepared choices contain only metadata beside their alternatives. Keep
%% those wrappers and their order while carrying common fields to each leaf.
%% Reject definition scopes that could rebind refs in the common fields.
-spec intersect_object_(json_schema(), json_schema()) -> json_schema().
intersect_object_(Common, #{<<"default">> := _} = Branch) ->
    %% A branch fallback could omit common fields, even on a nested choice.
    error({klsn_rule_generator, unsupported_intersection, {Common, Branch}});
intersect_object_(Common, #{<<"anyOf">> := []} = Branch) ->
    error({klsn_rule_generator, unsupported_intersection, {Common, Branch}});
intersect_object_(Common, #{<<"oneOf">> := []} = Branch) ->
    error({klsn_rule_generator, unsupported_intersection, {Common, Branch}});
intersect_object_(Common, #{<<"anyOf">> := Alternatives} = Branch) ->
    ensure_mergeable_object_(maps:remove(<<"anyOf">>, Branch)),
    Branch#{<<"anyOf">> := lists:map(fun(Alternative) ->
        intersect_object_(Common, Alternative)
    end, Alternatives)};
intersect_object_(Common, #{<<"oneOf">> := Alternatives} = Branch) ->
    ensure_mergeable_object_(maps:remove(<<"oneOf">>, Branch)),
    Branch#{<<"oneOf">> := lists:map(fun(Alternative) ->
        intersect_object_(Common, Alternative)
    end, Alternatives)};
intersect_object_(Common, Branch) ->
    ensure_mergeable_object_(Branch),
    %% Properties alone do not require an object. A nested leaf may supply
    %% the type when the common constraints only declare properties.
    MaybeType = case klsn_map:lookup([<<"type">>], Common) of
        none ->
            klsn_map:lookup([<<"type">>], Branch);
        Value ->
            Value
    end,
    case MaybeType of
        {value, <<"object">>} ->
            merge_object_(Common, Branch);
        _ ->
            error({klsn_rule_generator, unsupported_intersection,
                   {Common, Branch}})
    end.

%% Closed objects and schema-valued additionalProperties are scoped to the
%% original property set. Unioning their properties would weaken constraints,
%% so refuse them here rather than silently changing that scope. References
%% and other combinators at this intersection level likewise need a more
%% general intersection representation; property-level refs remain supported.
-spec ensure_mergeable_object_(json_schema()) -> ok.
ensure_mergeable_object_(Schema) when is_map(Schema) ->
    Allowed = lists:all(fun({Key, Value}) ->
        case Key of
            <<"type">> ->
                Value =:= <<"object">>;
            <<"properties">> ->
                is_map(Value);
            <<"required">> ->
                is_list(Value);
            <<"additionalProperties">> ->
                Value =:= true;
            _ ->
                annotation_key_(Key)
        end
    end, maps:to_list(Schema)),
    case Allowed of
        true ->
            ok;
        false ->
            error({klsn_rule_generator, unsupported_intersection, Schema})
    end;
ensure_mergeable_object_(Schema) ->
    error({klsn_rule_generator, unsupported_intersection, Schema}).

-spec merge_object_(json_schema(), json_schema()) -> json_schema().
merge_object_(Common, Branch) ->
    Properties = maps:fold(fun(Key, Value, Acc) ->
        maps:update_with(Key, fun(Old) ->
            intersect_property_(Old, Value)
        end, Value, Acc)
    end, maps:get(<<"properties">>, Common, #{}), maps:get(<<"properties">>, Branch, #{})),
    Required = lists:usort(maps:get(<<"required">>, Common, []) ++ maps:get(<<"required">>, Branch, [])),
    (maps:merge(Common, Branch))#{
        <<"type">> => <<"object">>
      , <<"properties">> => Properties
      , <<"required">> => Required
    }.

%% Restrict overlapping properties to intersections we can represent without
%% losing either constraints or normalized fields. In particular, all_of of
%% two structs does not combine their outputs.
-spec intersect_property_(json_schema(), json_schema()) -> json_schema().
intersect_property_(Schema, Schema) ->
    Schema;
intersect_property_(true, Schema) ->
    Schema;
intersect_property_(Schema, true) ->
    Schema;
intersect_property_(false, _) ->
    false;
intersect_property_(_, false) ->
    false;
intersect_property_(Common, Branch) when is_map(Common), is_map(Branch) ->
    ensure_property_intersection_(Common),
    ensure_property_intersection_(Branch),
    %% Default rules return fallbacks without validating them. Moving a
    %% fallback across a different constraint could bypass that constraint.
    case maps:is_key(<<"default">>, Common) orelse maps:is_key(<<"default">>, Branch) of
        true ->
            error({klsn_rule_generator, unsupported_intersection, {Common, Branch}});
        false ->
            ok
    end,
    CommonType = klsn_map:lookup([<<"type">>], Common),
    BranchType = klsn_map:lookup([<<"type">>], Branch),
    case {CommonType, BranchType} of
        {{value, <<"object">>}, {value, <<"object">>}} ->
            merge_object_(Common, Branch);
        {Type, Type} ->
            intersect_scalar_(Common, Branch);
        {none, _} ->
            intersect_scalar_(Common, Branch);
        {_, none} ->
            intersect_scalar_(Common, Branch);
        _ ->
            error({klsn_rule_generator, unsupported_intersection, {Common, Branch}})
    end;
intersect_property_(Common, Branch) ->
    error({klsn_rule_generator, unsupported_intersection, {Common, Branch}}).

-spec ensure_property_intersection_(json_schema()) -> ok.
ensure_property_intersection_(Schema) ->
    case klsn_map:lookup([<<"type">>], Schema) of
        {value, <<"object">>} ->
            ensure_mergeable_object_(Schema);
        MaybeType ->
            AllowedTypes = [
                <<"string">>
              , <<"integer">>
              , <<"number">>
              , <<"boolean">>
              , <<"null">>
              , <<"float">>
            ],
            AllowedType = case MaybeType of
                none ->
                    true;
                {value, Type} ->
                    lists:member(Type, AllowedTypes)
            end,
            Allowed = AllowedType
                andalso lists:all(fun(Key) ->
                    Key =:= <<"type">> orelse Key =:= <<"enum">> orelse annotation_key_(Key)
                end, maps:keys(Schema))
                andalso (not maps:is_key(<<"enum">>, Schema)
                    orelse MaybeType =:= none orelse MaybeType =:= {value, <<"string">>}),
            case Allowed of
                true ->
                    ok;
                false ->
                    error({klsn_rule_generator, unsupported_intersection, Schema})
            end
    end.

-spec intersect_scalar_(json_schema(), json_schema()) -> json_schema().
intersect_scalar_(Common, Branch) ->
    Merged = maps:merge(Common, Branch),
    %% Check the combined type too (e.g. an untyped string enum plus integer).
    ensure_property_intersection_(Merged),
    CommonEnum = klsn_map:lookup([<<"enum">>], Common),
    BranchEnum = klsn_map:lookup([<<"enum">>], Branch),
    case {CommonEnum, BranchEnum} of
        {{value, CommonValues0}, {value, BranchValues0}} ->
            CommonValues = klsn_rule:normalize(CommonValues0, {list, binstr}),
            BranchValues = klsn_rule:normalize(BranchValues0, {list, binstr}),
            Values = lists:filter(fun(Value) ->
                lists:member(Value, BranchValues)
            end, CommonValues),
            Merged#{<<"enum">> := Values};
        _ ->
            Merged
    end.
