-module(klsn_io_SUITE).
-include_lib("common_test/include/ct.hrl").

%% Export the suite callback and test cases
-export([all/0,
         format/1,
         get_line/1]).

%% Define the test suite with all/0
all() ->
    [
        format,
        get_line
    ].

%% Just make sure there is no error

format(_Config) ->
    ct:print("\n\n\n\n\n\n"),
    klsn_io:format("").

get_line(_Config) ->
    Pid = self(),
    Ref = make_ref(),
    spawn_link(fun() ->
        klsn_io:get_line(),
        Pid ! {Ref, ok}
    end),
    receive
        {Ref, ok} ->
            ok
    after 1000 ->
        timeout
    end.

