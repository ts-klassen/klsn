-module(klsn_code).

-export_type([
        sample_type/0
      , sample_type/1
      , sample_type/2
    ]).

-export([
        sample_function/0
      , sample_function/1
      , type/1
      , spec/1
      , function/1
    ]).

-type sample_type() :: sample_type_payload.
-type sample_type(A) :: A.
-type sample_type(A, B) :: {A,B}.

-spec sample_function() -> sample_function_return.
sample_function() ->
    sample_function_return.

-spec sample_function(sample_function_arg1) -> sample_function_return1;
                     (sample_function_arg2) -> sample_function_return2.
sample_function(sample_function_arg1) ->
    sample_function_return1;
sample_function(sample_function_arg2) ->
    sample_function_return2.

-spec type(mfa()) -> any(). % FIXME: type any should not be used.
type(_MFA) ->
    todo.

-spec spec(mfa()) -> any(). % FIXME: type any should not be used.
spec(_MFA) ->
    todo.

-spec function(mfa()) -> any(). % FIXME: type any should not be used.
function(_MFA) ->
    todo.

abstract_code(Module) ->
    {ok, Bin} = code:get_object_code(gpte_image),
    beam_lib:chunks(Bin, [abstract_code]).

