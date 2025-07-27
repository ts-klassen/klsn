-module(klsn_list).

-export([
        pmap/2
      , pmap/3
    ]).

-export_type([
        workers/0
    ]).

%% The maximum amount of worker processes that may execute jobs at the
%% same time when using pmap/3.
-type workers() :: pos_integer().


%% @doc
%% Parallel version of lists:map/2 that applies Fun to every element of
%% In and returns a list of results preserving the original order.
%%
%% The implementation spawns one transient process per element. When one
%% of those processes crashes, the error is propagated back to the caller
%% (so that ?assertError in the test-suite behaves as expected).
-spec pmap(fun((T) -> R), [T]) -> [R] when
        T :: term(),
        R :: term().
pmap(Fun, List) ->
    pmap(Fun, List, #{}).


%% @doc
%% Same as pmap/2 but lets the caller restrict the amount of concurrent
%% workers by passing #{workers => N}.  When the option is omitted, one
%% worker per list element is used.
-spec pmap(fun((T) -> R), [T], #{workers => workers()}) -> [R] when
        T :: term(),
        R :: term().
pmap(_Fun, [], _Opts) ->
    [];
pmap(Fun, List, Opts) when is_list(List), is_map(Opts) ->
    WorkersOpt = maps:get(workers, Opts, unlimited),
    Workers = case WorkersOpt of
        unlimited -> length(List);
        N when is_integer(N), N > 0 -> N;
        _ -> erlang:error(badarg, [Fun, List, Opts])
    end,
    TRef = make_ref(),
    Parent = self(),
    Indexed = lists:zip(lists:seq(1, length(List)), List),
    {Running, Queue} = lists:split(min(Workers, length(Indexed)), Indexed),
    InFlight = spawn_tasks(Running, Fun, Parent, TRef, #{}),
    collect(Queue, InFlight, #{}, length(List), TRef, Fun, List, Opts).


%% Spawn all tasks in Tasks and return a map RefMap of monitor-ref to
%% Index so we can identify the DOWN messages.
-spec spawn_tasks([{integer(), term()}], fun((term()) -> term()), pid(), reference(),
    maps:map(reference(), integer())) -> maps:map(reference(), integer()).
spawn_tasks([], _Fun, _Parent, _Tag, RefMap) ->
    RefMap;
spawn_tasks([{Index, Elem}|Rest], Fun, Parent, Tag, RefMap) ->
    {_, MRef} = erlang:spawn_monitor(fun()->
        Result = Fun(Elem),
        Parent ! {Tag, Index, Result}
    end),
    spawn_tasks(Rest, Fun, Parent, Tag, RefMap#{MRef => Index}).


%% Main loop that receives results or crashes, spawns queued work, and
%% finally assembles the ordered result list.
-spec collect([{integer(), term()}], maps:map(reference(), integer()),
              maps:map(integer(), term()), non_neg_integer(), reference(),
              fun((term()) -> term()), [term()], map()) -> [term()].
collect(_Queue, _RefMap, Acc, Expected, _Tag, _Fun, _List, _Opts) when map_size(Acc) =:= Expected ->
    %% All results collected – build ordered list and return.
    lists:map(fun(I) -> maps:get(I, Acc) end, lists:seq(1, Expected));
collect(Queue, RefMap, Acc, Expected, Tag, Fun, List, Opts) ->
    receive
        {Tag, Index, Result} ->
            %% Successful completion of a job.
            {NewRefMap, NextQueue} = maybe_spawn_next(Queue, RefMap, Fun, Tag),
            collect(NextQueue, NewRefMap, Acc#{Index => Result}, Expected,
                    Tag, Fun, List, Opts);
        {'DOWN', MRef, process, _Pid, Reason} ->
            case maps:take(MRef, RefMap) of
                {_, NewRefMap} when Reason =:= normal ->
                    %% Task completed successfully – ignore, it has sent its
                    %% own {Tag, …} message already.
                    collect(Queue, NewRefMap, Acc, Expected, Tag, Fun, List, Opts);
                {_, _NewRefMap} ->
                    %% An error occurred in a worker – propagate only the
                    %% primary reason (omit the embedded stacktrace when the
                    %% exit reason is a two-tuple {Reason, Stack}).
                    case Reason of
                        {R, _} -> erlang:error(R);
                        R -> erlang:error(R)
                    end;
                error ->
                    %% Unknown monitor – shouldn't happen.
                    collect(Queue, RefMap, Acc, Expected, Tag, Fun, List, Opts)
            end
    end.


%% Spawn the next task from Queue if available, update the RefMap and
%% return {NewRefMap, NewQueue}.
-spec maybe_spawn_next([{integer(), term()}], maps:map(reference(), integer()),
    fun((term()) -> term()), reference()) -> {maps:map(reference(), integer()), [{integer(), term()}]}.
maybe_spawn_next([], RefMap, _Fun, _Tag) ->
    {RefMap, []};
maybe_spawn_next([{Index, Elem}|Rest], RefMap, Fun, Tag) ->
    Parent = self(),
    {_, MRef} = erlang:spawn_monitor(fun()->
        Result = Fun(Elem),
        Parent ! {Tag, Index, Result}
    end),
    {RefMap#{MRef => Index}, Rest}.
