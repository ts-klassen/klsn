-module(klsn_list_tests).
-include_lib("eunit/include/eunit.hrl").

pmap_order_test() ->
    In = [1,2,3,4,5],
    F  = fun(X) -> X * 2 end,
    Exp = [2,4,6,8,10],
    ?assertEqual(Exp, klsn_list:pmap(F, In)),
    ?assertEqual(Exp, klsn_list:pmap(F, In, #{workers => 2})).

pmap_pid_test() ->
    Parent = self(),
    In = [1,2,3,4,5],
    Res = klsn_list:pmap(fun(X) -> {X, self()} end, In),
    Pids = lists:map(fun({_, P}) -> P end, Res),
    ?assert(not lists:any(fun(P) -> P =:= Parent end, Pids)),
    Set = sets:from_list(Pids, [{version, 2}]),
    ?assertEqual(length(Pids), sets:size(Set)).

pmap_timing_unlimited_test() ->
    Sleep = 100,
    In = [1,2,3,4],
    Fun = fun(_) -> timer:sleep(Sleep), ok end,
    {Micros, _} = timer:tc(klsn_list, pmap, [Fun, In]),
    Milli = Micros div 1000,
    %% Allow generous headroom: execution should finish well under
    %% Sleep * 3 even on moderately slow systems.
    ?assert(Milli < Sleep * 3).

pmap_timing_bounded_test() ->
    Sleep = 100,
    In = [1,2,3,4],
    Workers = 2,
    Fun = fun(_) -> timer:sleep(Sleep), ok end,
    {Micros, _} = timer:tc(klsn_list, pmap, [Fun, In, #{workers => Workers}]),
    Milli = Micros div 1000,
    Rounds = (length(In) + Workers - 1) div Workers, %% ceiling division
    Expected = Rounds * Sleep,
    Lower = Expected div 2,
    Upper = Expected * 3,
    ?assert(Milli >= Lower),
    ?assert(Milli =< Upper).

pmap_empty_test() ->
    ?assertEqual([], klsn_list:pmap(fun(X) -> X end, [])).

pmap_error_test() ->
    Bad = fun(3) -> error(bad_elem);
             (X) -> X
          end,
    ?assertError(bad_elem, klsn_list:pmap(Bad, [1,2,3,4])).
