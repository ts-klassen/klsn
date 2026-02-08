-module(klsn_rule_generator).
-include_lib("klsn/include/klsn_rule_annotation.hrl").

-export([
        from_json_schema/1
    ]).

-klsn_rule_alias([
        {json_schema, {any_of, [
            {alias, {?MODULE, json_schema_true}},
            {alias, {?MODULE, json_schema_false}},
            {alias, {?MODULE, json_schema_any_of}},
            {alias, {?MODULE, json_schema_all_of}},
            {alias, {?MODULE, json_schema_one_of}},
            {alias, {?MODULE, json_schema_integer}},
            {alias, {?MODULE, json_schema_string}},
            {alias, {?MODULE, json_schema_boolean}},
            {alias, {?MODULE, json_schema_number}},
            {alias, {?MODULE, json_schema_float}},
            {alias, {?MODULE, json_schema_null}},
            {alias, {?MODULE, json_schema_array}},
            {alias, {?MODULE, json_schema_const}},
            {alias, {?MODULE, json_schema_enum}},
            {alias, {?MODULE, json_schema_object}}
        ]}}
      , {json_schema_true, {exact, true}}
      , {json_schema_false, {exact, false}}
      , {json_schema_any_of, {struct, #{
            anyOf => {required, {list, {alias, {?MODULE, json_schema}}}},
            default => {optional, {alias, {?MODULE, json_value}}}
        }}}
      , {json_schema_all_of, {struct, #{
            allOf => {required, {list, {alias, {?MODULE, json_schema}}}},
            default => {optional, {alias, {?MODULE, json_value}}}
        }}}
      , {json_schema_one_of, {struct, #{
            oneOf => {required, {list, {alias, {?MODULE, json_schema}}}},
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
            default => {optional, {alias, {?MODULE, json_value}}}
        }}}
      , {json_schema_string, {struct, #{
            type => {required, {enum, [string]}},
            default => {optional, {alias, {?MODULE, json_value}}}
        }}}
      , {json_schema_boolean, {struct, #{
            type => {required, {enum, [boolean]}},
            default => {optional, {alias, {?MODULE, json_value}}}
        }}}
      , {json_schema_number, {struct, #{
            type => {required, {enum, [number]}},
            default => {optional, {alias, {?MODULE, json_value}}}
        }}}
      , {json_schema_float, {struct, #{
            type => {required, {enum, [float]}},
            default => {optional, {alias, {?MODULE, json_value}}}
        }}}
      , {json_schema_null, {struct, #{
            type => {required, {enum, [null]}},
            default => {optional, {alias, {?MODULE, json_value}}}
        }}}
      , {json_schema_array, {struct, #{
            type => {required, {enum, [array]}},
            items => {optional, {alias, {?MODULE, json_schema}}},
            default => {optional, {alias, {?MODULE, json_value}}}
        }}}
      , {json_schema_const, {struct, #{
            const => {required, {any_of, [{exact, null}, boolean, number, binstr]}},
            default => {optional, {alias, {?MODULE, json_value}}}
        }}}
      , {json_schema_enum, {struct, #{
            enum => {required, {list, binstr}},
            default => {optional, {alias, {?MODULE, json_value}}}
        }}}
      , {json_schema_object, {struct, #{
            type => {required, {enum, [object]}}
          , properties => {required, {map, {binstr, {alias, {?MODULE, json_schema}}}}}
          , required => {optional, {list, binstr}}
          , default => {optional, {alias, {?MODULE, json_value}}}
        }}}
    ]).
-type json_schema() :: klsn_rule:alias(json_schema).

-klsn_rule_alias({json_schema_rules, {struct, #{
        from_json => {required, term}
      , to_json => {required, term}
    }}}).
-type json_schema_rules() :: #{
        from_json => klsn_rule:rule()
      , to_json => klsn_rule:rule()
    }.

-klsn_input_rule([{alias, {?MODULE, json_schema}}]).
-klsn_output_rule({alias, {?MODULE, json_schema_rules}}).
%% @doc
%% Do not accept JSON Schema from user input; this function uses binary_to_atom/2
%% when converting property names, which can exhaust the atom table.
%% Not for production use: generating rules at runtime is for development only.
%% Generate rules ahead of time and include them in releases.
-spec from_json_schema(json_schema()) -> #{from_json := klsn_rule:rule(), to_json := klsn_rule:rule()}.
from_json_schema(true) ->
    json_rules_from_rule_(term);
from_json_schema(false) ->
    json_rules_from_rule_({enum, []});
from_json_schema(#{anyOf := Schemas}=Schema) ->
    {FromRules, ToRules} = json_rule_list_(Schemas),
    #{from_json => with_default_(Schema, {any_of, FromRules}),
      to_json => with_default_(Schema, {any_of, ToRules})};
from_json_schema(#{allOf := Schemas}=Schema) ->
    {FromRules, ToRules} = json_rule_list_(Schemas),
    #{from_json => with_default_(Schema, {all_of, FromRules}),
      to_json => with_default_(Schema, {all_of, ToRules})};
from_json_schema(#{oneOf := Schemas}=Schema) ->
    {FromRules, ToRules} = json_rule_list_(Schemas),
    #{from_json => with_default_(Schema, {any_of, FromRules}),
      to_json => with_default_(Schema, {any_of, ToRules})};
from_json_schema(#{type := integer}=Schema) ->
    json_rules_from_rule_(with_default_(Schema, integer));
from_json_schema(#{type := string}=Schema) ->
    json_rules_from_rule_(with_default_(Schema, binstr));
from_json_schema(#{type := boolean}=Schema) ->
    json_rules_from_rule_(with_default_(Schema, boolean));
from_json_schema(#{type := number}=Schema) ->
    json_rules_from_rule_(with_default_(Schema, number));
from_json_schema(#{type := float}=Schema) ->
    json_rules_from_rule_(with_default_(Schema, float));
from_json_schema(#{type := null}=Schema) ->
    json_rules_from_rule_(with_default_(Schema, {exact, null}));
from_json_schema(#{type := array}=Schema) ->
    case klsn_map:lookup([items], Schema) of
        {value, Items} ->
            #{from_json := FromItem, to_json := ToItem} = from_json_schema(Items),
            #{from_json => with_default_(Schema, {list, FromItem}),
              to_json => with_default_(Schema, {list, ToItem})};
        none ->
            json_rules_from_rule_(with_default_(Schema, {list, term}))
    end;
from_json_schema(#{const := Const}=Schema) ->
    json_rules_from_rule_(with_default_(Schema, {exact, Const}));
from_json_schema(#{enum := Enum}=Schema) ->
    json_rules_from_rule_(with_default_(Schema, {enum, Enum}));
from_json_schema(#{type := object, properties := Properties}=Schema) ->
    Required = maps:get(required, Schema, []),
    PropList = maps:to_list(Properties),
    MissingRequired = lists:filter(fun(Key) ->
        not maps:is_key(Key, Properties)
    end, Required),
    case MissingRequired of
        [] ->
            {FromMap, ToMap} = lists:foldl(fun({PropName, PropSchema}, {FromAcc, ToAcc}) ->
                #{from_json := FromJson, to_json := ToJson} = from_json_schema(PropSchema),
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
    end;
from_json_schema(Schema) ->
    error({klsn_rule_generator, unsupported_schema, Schema}).

json_rules_from_rule_(Rule) ->
    #{from_json => Rule, to_json => Rule}.

json_rule_list_(Schemas) ->
    {FromRev, ToRev} = lists:foldl(fun(ItemSchema, {FromAcc, ToAcc}) ->
        #{from_json := FromRule, to_json := ToRule} = from_json_schema(ItemSchema),
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
