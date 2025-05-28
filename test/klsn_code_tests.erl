-module(klsn_code_tests).
-include_lib("eunit/include/eunit.hrl").

%% Tests for type/1
type_1_test() ->
    ?assertMatch(
        {sample_type,{atom,{_,_},sample_type_payload},[]}
      , klsn_code:type({klsn_code, sample_type, 0})),
    ?assertMatch(
        {sample_type,{var,{_,_},'A'},[{var,{_,_},'A'}]}
      , klsn_code:type({klsn_code, sample_type, 1})),
    ?assertMatch(
        {sample_type,
            {type,
                {_,_},
                tuple,
                [{var,{_,_},'A'},{var,{_,_},'B'}]},
            [{var,{_,_},'A'},{var,{_,_},'B'}]}
      , klsn_code:type({klsn_code, sample_type, 2})),
    ok.

%% Tests for spec/1
spec_1_test() ->
    ?assertMatch(
        [{type,
             {_,_},
             'fun',
             [{type,{_,_},product,[]},
              {atom,{_,_},sample_function_return}]}]
      , klsn_code:spec({klsn_code, sample_function, 0})),
    ?assertMatch(
        [{type,
             {_,_},
             'fun',
             [{type,
                  {_,_},
                  product,
                  [{atom,{_,_},sample_function_arg1}]},
              {atom,{_,_},sample_function_return1}]},
         {type,
             {_,_},
             'fun',
             [{type,
                  {_,_},
                  product,
                  [{atom,{_,_},sample_function_arg2}]},
              {atom,{_,_},sample_function_return2}]}]
      , klsn_code:spec({klsn_code, sample_function, 1})),
    ok.

%% Tests for function/1
function_1_test() ->
    ?assertMatch(
        [{clause,
             {_,_},
             [],[],
             [{atom,{_,_},sample_function_return}]}]
      , klsn_code:function({klsn_code, sample_function, 0})),
    ?assertMatch(
        [{clause,
             {_,_},
             [{atom,{_,_},sample_function_arg1}],
             [],
             [{atom,{_,_},sample_function_return1}]},
         {clause,
             {_,_},
             [{atom,{_,_},sample_function_arg2}],
             [],
             [{atom,{_,_},sample_function_return2}]}]
      , klsn_code:function({klsn_code, sample_function, 1})),
    ok.

