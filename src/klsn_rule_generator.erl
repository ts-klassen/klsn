-module(klsn_rule_generator).

-export([from_json_schema/1]).

-include_lib("klsn/include/klsn_rule_annotation.hrl").

-define(JSON_SCHEMA_RULE, {struct, #{type => {required, {enum, [integer]}}}}).

-type json_schema() :: map().

-spec from_json_schema(json_schema()) -> #{from_json := klsn_rule:rule(), to_json := klsn_rule:rule()}.
-klsn_input_rule([?JSON_SCHEMA_RULE]).
from_json_schema(#{type := integer}) ->
    #{from_json => integer, to_json => integer};
from_json_schema(Schema) ->
    error({klsn_rule_generator, unsupported_schema, Schema}).
