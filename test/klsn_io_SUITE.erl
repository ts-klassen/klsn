-module(klsn_io_SUITE).
-include_lib("common_test/include/ct.hrl").

%% Export the suite callback and test cases
-export([suite/0,
         test_format_1/1,
         test_format_2/1,
         test_get_line_0/1,
         test_get_line_1/1]).

%% Define the test suite with suite/0
suite() ->
    [
        {setup, setup_io, teardown_io},
        test_format_1,
        test_format_2,
        test_get_line_0,
        test_get_line_1
    ].

%% Setup function to spawn the IO server and set group leader
setup_io(_) ->
    %% Spawn a new process to act as the IO server
    Pid = spawn(fun io_server/0),
    %% Get current group leader
    OldGL = erlang:group_leader(),
    %% Set the group leader to the IO server
    erlang:group_leader(Pid, self()),
    %% Return the IO server PID and old group leader in the state map
    {ok, #{io_pid => Pid, old_gl => OldGL}}.

%% Teardown function to restore group leader and stop the IO server
teardown_io(State) ->
    %% Restore the old group leader
    OldGL = maps:get(old_gl, State),
    erlang:group_leader(OldGL, self()),
    %% Send a stop message to terminate the IO server
    Pid = maps:get(io_pid, State),
    Pid ! stop,
    %% Allow some time for the process to terminate gracefully
    timer:sleep(100),
    ok.

%% IO server process to capture and relay output
io_server() ->
    receive
        {io_request, From, Ref, {put_chars, Device, Chars}} ->
            %% Send the received characters back to the test process
            From ! {io_reply, Ref, Chars},
            io_server();
        {io_request, From, Ref, {get_line, Prompt}} ->
            %% Simulate user input by sending a predefined response
            From ! {io_reply, Ref, "Simulated input\n"},
            io_server();
        stop ->
            %% Terminate the IO server gracefully
            ok
    after 5000 ->
            ok
    end.

%% Test case for format/1
test_format_1(_Context) ->
    %% Define the format string as a binary
    Format = <<"Hello, Common Test!\n">>,
    %% Call the format/1 function
    klsn_io:format("Hello, Common Test!\n"),
    %% Receive the output from the IO server
    receive
        {io_reply, _, Output} when Output =:= Format ->
            ok;
        {io_reply, _, _Output} ->
            ct:fail("format/1 did not output the expected string");
        _ ->
            ct:fail("Unexpected message received in test_format_1")
    after 1000 ->
            ct:fail("No output received from format/1")
    end.

%% Test case for format/2
test_format_2(_Context) ->
    %% Define the format string and data
    Format = <<"Number: ~p, String: ~s~n">>,
    Data = [42, <<"Erlang">>],
    %% Expected output after formatting
    ExpectedOutput = <<"Number: 42, String: Erlang\n">>,
    %% Call the format/2 function
    klsn_io:format("Number: ~p, String: ~s~n", [42, "Erlang"]),
    %% Receive the output from the IO server
    receive
        {io_reply, _, Output} when Output =:= ExpectedOutput ->
            ok;
        {io_reply, _, _Output} ->
            ct:fail("format/2 did not output the expected formatted string");
        _ ->
            ct:fail("Unexpected message received in test_format_2")
    after 1000 ->
            ct:fail("No output received from format/2")
    end.

%% Test case for get_line/0
test_get_line_0(_Context) ->
    %% Call the get_line/0 function
    Line = klsn_io:get_line(),
    %% Define the expected simulated input as a binary
    ExpectedLine = <<"Simulated input\n">>,
    %% Verify the retrieved line using pattern matching
    case Line of
        ExpectedLine ->
            ok;
        _ ->
            ct:fail("get_line/0 did not return the expected input")
    end.

%% Test case for get_line/1
test_get_line_1(_Context) ->
    %% Define the prompt as a binary
    Prompt = <<"Enter something: ">>,
    %% Call the get_line/1 function with the prompt
    Line = klsn_io:get_line(Prompt),
    %% Define the expected simulated input as a binary
    ExpectedLine = <<"Simulated input\n">>,
    %% Verify the retrieved line using pattern matching
    case Line of
        ExpectedLine ->
            ok;
        _ ->
            ct:fail("get_line/1 did not return the expected input")
    end.
