-module(klsn_time).

-export([
        rfc3339_to_unix_seconds/1
      , rfc3339_to_unix_nanoseconds/1
      , unix_seconds_to_rfc3339/1
      , unix_nanoseconds_to_rfc3339/1
      , unix_seconds_to_unix_nanoseconds/1
      , unix_nanoseconds_to_unix_seconds/1
    ]).

-export_type([
        rfc3339/0
      , unix_seconds/0
      , unix_nanoseconds/0
    ]).

-type rfc3339() :: unicode:unicode_binary().
-type unix_seconds() :: integer().
-type unix_nanoseconds() :: integer().


%% @doc
%% Convert an RFC3339 datetime (binary) to Unix time in seconds.
-spec rfc3339_to_unix_seconds(rfc3339()) -> unix_seconds().
rfc3339_to_unix_seconds(RFC3339) when is_binary(RFC3339) ->
    String = binary_to_list(RFC3339),
    calendar:rfc3339_to_system_time(String, [{unit, second}]).

%% @doc
%% Convert an RFC3339 datetime (binary) to Unix time in nanoseconds.
-spec rfc3339_to_unix_nanoseconds(rfc3339()) -> unix_nanoseconds().
rfc3339_to_unix_nanoseconds(RFC3339) when is_binary(RFC3339) ->
    String = binary_to_list(RFC3339),
    calendar:rfc3339_to_system_time(String, [{unit, nanosecond}]).

%% @doc
%% Convert Unix time in seconds to RFC3339 datetime (binary).
-spec unix_seconds_to_rfc3339(unix_seconds()) -> rfc3339().
unix_seconds_to_rfc3339(Seconds) when is_integer(Seconds) ->
    iolist_to_binary(calendar:system_time_to_rfc3339(Seconds, [{unit, second}, {offset, "+09:00"}])).

%% @doc
%% Convert Unix time in nanoseconds to RFC3339 datetime (binary).
-spec unix_nanoseconds_to_rfc3339(unix_nanoseconds()) -> rfc3339().
unix_nanoseconds_to_rfc3339(Nanoseconds) when is_integer(Nanoseconds) ->
    iolist_to_binary(calendar:system_time_to_rfc3339(Nanoseconds, [{unit, nanosecond}, {offset, "+09:00"}])).

%% @doc
%% Convert Unix time from seconds to nanoseconds.
-spec unix_seconds_to_unix_nanoseconds(unix_seconds()) -> unix_nanoseconds().
unix_seconds_to_unix_nanoseconds(Seconds) when is_integer(Seconds) ->
    erlang:convert_time_unit(Seconds, second, nanosecond).

%% @doc
%% Convert Unix time from nanoseconds to seconds (truncating toward zero).
-spec unix_nanoseconds_to_unix_seconds(unix_nanoseconds()) -> unix_seconds().
unix_nanoseconds_to_unix_seconds(Nanoseconds) when is_integer(Nanoseconds) ->
    erlang:convert_time_unit(Nanoseconds, nanosecond, second).
