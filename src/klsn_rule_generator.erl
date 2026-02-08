-module(klsn_rule_generator).
-include_lib("klsn/include/klsn_rule_annotation.hrl").

-export([
        from_json_schema/1
    ]).

-klsn_rule_alias([
        {json_schema, {any_of, [
            {alias, {?MODULE, json_schema_integer}},
            {alias, {?MODULE, json_schema_object}}
        ]}}
      , {json_schema_integer, {struct, #{
            type => {required, {enum, [integer]}}
        }}}
      , {json_schema_object, {struct, #{
            type => {required, {enum, [object]}}
          , properties => {required, {map, {binstr, {alias, {?MODULE, json_schema}}}}}
          , required => {optional, {list, binstr}}
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
from_json_schema(#{type := integer}) ->
    #{from_json => integer, to_json => integer};
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
            #{from_json => {struct, FromMap}, to_json => {struct, ToMap}};
        _ ->
            error({klsn_rule_generator, unsupported_schema, Schema})
    end;
from_json_schema(Schema) ->
    error({klsn_rule_generator, unsupported_schema, Schema}).
