-module(klsn_io).

-export([
        format/1
      , format/2
      , get_line/0
      , get_line/1
    ]).

-spec format(io:format()) -> ok.
format(Format) ->
    format(Format, []).

-spec format(io:format(), [term()]) -> ok.
format(Format, Data) ->
    io:format(device(), Format, Data).

-spec get_line() -> unicode:unicode_binary() | io:server_no_data().
get_line() ->
    get_line(<<>>).

-spec get_line(io:prompt()) -> unicode:unicode_binary() | io:server_no_data().
get_line(Prompt) ->
    io:get_line(device(), Prompt).

-spec device() -> io:device().
device() ->
    Device = erlang:group_leader(),
    io:setopts(Device, [{encoding,utf8}, {binary, true}]),
    Device.

