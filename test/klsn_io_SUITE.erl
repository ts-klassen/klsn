-module(klsn_io_SUITE).
-include_lib("common_test/include/ct.hrl").

%% Export the suite callback
-export([suite/0]).

%% Export test cases
-export([test_format_1/1,
         test_format_2/1,
         test_get_line_0/1,
         test_get_line_1/1]).

%% Import necessary modules
-define(TEST_DEVICE, "ct_device").

%% Suite callback to define the list of tests
suite() ->
    [context, %% Ensure each test runs in isolation
     {setup, setup_suite, teardown_suite},
     test_format_1,
     test_format_2,
     test_get_line_0,
     test_get_line_1].

%% Setup function to configure the IO device for testing
setup_suite(_) ->
    %% Create a new IO device for testing
    Device = io:format("~p~n", [self()]),
    io:setopts(Device, [{encoding, utf8}, {binary, true}]),
    {ok, #{device => Device}}.

%% Teardown function to clean up after tests
teardown_suite(State) ->
    io:close(State#{
        device => Device}).

%% Test case for format/1
test_format_1(Context) ->
    %% Capture the output sent to the test device
    Device = maps:get(device, Context),
    %% Define a message to format
    Message = "Hello, World!",
    %% Expected output
    ExpectedOutput = "Hello, World!",
    %% Send the format message
    klsn_io:format(Message),
    %% Receive the output
    receive
        Output ->
            ?assertEqual(<<ExpectedOutput/binary>>, Output)
    after 1000 ->
            ct:fail("No output received from format/1")
    end.

%% Test case for format/2
test_format_2(Context) ->
    Device = maps:get(device, Context),
    %% Define a format string and data
    Format = "Number: ~p, String: ~s~n",
    Data = [42, "Erlang"],
    %% Expected output
    ExpectedOutput = "Number: 42, String: Erlang\n",
    %% Send the formatted message
    klsn_io:format(Format, Data),
    %% Receive the output
    receive
        Output ->
            ?assertEqual(<<ExpectedOutput/binary>>, Output)
    after 1000 ->
            ct:fail("No output received from format/2")
    end.

%% Test case for get_line/0
test_get_line_0(_) ->
    %% Simulate user input by sending a message to the process
    TestInput = "Test line without prompt\n",
    self() ! {io_request, self(), make_ref(), {get_line, <<>>}},
    %% Send the input
    receive
        {io_reply, _, Line} ->
            ?assertEqual("Test line without prompt\n", Line)
    after 1000 ->
            ct:fail("No response received from get_line/0")
    end.

%% Test case for get_line/1
test_get_line_1(_) ->
    %% Define a prompt and expected input
    Prompt = "Enter something: ",
    TestInput = "User input via prompt\n",
    ExpectedOutput = "User input via prompt\n",
    %% Simulate user input
    self() ! {io_request, self(), make_ref(), {get_line, Prompt}},
    %% Send the input
    receive
        {io_reply, _, Line} ->
            ?assertEqual(ExpectedOutput, Line)
    after 1000 ->
            ct:fail("No response received from get_line/1")
    end.
