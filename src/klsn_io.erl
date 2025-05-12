-module(klsn_io).

-export([
        format/1
      , format/2
      , get_line/0
      , get_line/1
    ]).

%% @doc
%% Print *Format* (an `io:format()` compatible string) to the current
%% group leader using UTF-8 encoding.
-spec format(io:format()) -> ok.
format(Format) ->
    format(Format, []).

%% @doc
%% Same as `format/1` but with an explicit *Data* list that will be
%% interpolated into *Format*.
-spec format(io:format(), [term()]) -> ok.
format(Format, Data) ->
    io:format(device(), Format, Data).

 %% @doc
%% Read a single line from STDIN without printing any prompt. The returned
%% value is a UTF-8 binary or the atom returned by `io:get_line/2` when the
%% server signals end-of-file.
-spec get_line() -> unicode:unicode_binary() | io:server_no_data().
get_line() ->
    get_line(<<>>).

 %% @doc
%% Read a single line from STDIN after outputting *Prompt*.
-spec get_line(io:prompt()) -> unicode:unicode_binary() | io:server_no_data().
get_line(Prompt) ->
    io:get_line(device(), Prompt).

-spec device() -> io:device().
device() ->
    Device = erlang:group_leader(),
    io:setopts(Device, [{encoding,utf8}, {binary, true}]),
    Device.

