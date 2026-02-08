-module(klsn_rule_generator).
-include_lib("klsn/include/klsn_rule_annotation.hrl").

-export([
        from_json_schema/1
    ]).

-klsn_rule_alias({json_schema, {struct, #{
        type => {required, {enum, [integer]}}
    }}}).
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
-spec from_json_schema(json_schema()) -> #{from_json := klsn_rule:rule(), to_json := klsn_rule:rule()}.
from_json_schema(#{type := integer}) ->
    #{from_json => integer, to_json => integer};
from_json_schema(Schema) ->
    error({klsn_rule_generator, unsupported_schema, Schema}).
