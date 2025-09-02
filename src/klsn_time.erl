%% @doc
%% Time conversion utilities between RFC3339 datetimes and Unix time
%% (seconds/nanoseconds). This module:
%%
%% - Accepts RFC3339 datetimes as UTF-8 binaries (e.g. &lt;&lt;"1970-01-01T00:00:00Z"&gt;&gt;).
%% - Produces RFC3339 binaries using a fixed offset of "+09:00" for
%%   unix_seconds_to_rfc3339/1 and unix_nanoseconds_to_rfc3339/1.
%% - Supports timestamps before and after the Unix epoch.
%% - Uses the runtime's calendar and erlang:convert_time_unit/3 for
%%   correctness and performance.
%%
%% Notes on offsets and negativity:
%% - When parsing RFC3339 (the rfc3339_to_* functions), any explicit offset
%%   in the input is honored; the returned Unix time is always relative to UTC.
%% - For negative nanoseconds to seconds conversion, erlang:convert_time_unit/3
%%   follows floor semantics (rounding toward negative infinity). For example,
%%   -1 ns -> -1 s, and -1999999999 ns -> -2 s.
%%
%% @see rfc3339_to_unix_seconds/1
%% @see rfc3339_to_unix_nanoseconds/1
%% @see unix_seconds_to_rfc3339/1
%% @see unix_nanoseconds_to_rfc3339/1
%% @see unix_seconds_to_unix_nanoseconds/1
%% @see unix_nanoseconds_to_unix_seconds/1
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
%% Convert an RFC3339 datetime (binary) to Unix time (seconds since epoch).
%%
%% The input may include a timezone designator (e.g. Z, +09:00, -05:00).
%% The returned integer is the UTC Unix timestamp in seconds.
%%
%% Example:
%% <pre>
%% 1> klsn_time:rfc3339_to_unix_seconds(&lt;&lt;"1970-01-01T00:00:00Z"&gt;&gt;).
%% 0
%% 2> klsn_time:rfc3339_to_unix_seconds(&lt;&lt;"1970-01-01T09:00:00+09:00"&gt;&gt;).
%% 0
%% </pre>
-spec rfc3339_to_unix_seconds(rfc3339()) -> unix_seconds().
rfc3339_to_unix_seconds(RFC3339) when is_binary(RFC3339) ->
    String = binary_to_list(RFC3339),
    calendar:rfc3339_to_system_time(String, [{unit, second}]).

%% @doc
%% Convert an RFC3339 datetime (binary) to Unix time (nanoseconds since epoch).
%%
%% Fractional seconds in the input are supported up to nanosecond precision.
%% The returned integer is the UTC Unix timestamp in nanoseconds.
%%
%% Example:
%% <pre>
%% 1> klsn_time:rfc3339_to_unix_nanoseconds(&lt;&lt;"1970-01-01T00:00:00.123456789Z"&gt;&gt;).
%% 123456789
%% 2> klsn_time:rfc3339_to_unix_nanoseconds(&lt;&lt;"1970-01-01T08:59:59.500000000+09:00"&gt;&gt;).
%% -1500000000
%% </pre>
-spec rfc3339_to_unix_nanoseconds(rfc3339()) -> unix_nanoseconds().
rfc3339_to_unix_nanoseconds(RFC3339) when is_binary(RFC3339) ->
    String = binary_to_list(RFC3339),
    calendar:rfc3339_to_system_time(String, [{unit, nanosecond}]).

%% @doc
%% Convert Unix time in seconds to RFC3339 datetime (binary).
%%
%% The returned RFC3339 string uses the fixed offset "+09:00".
%%
%% Example:
%% <pre>
%% 1> klsn_time:unix_seconds_to_rfc3339(0).
%% &lt;&lt;"1970-01-01T09:00:00+09:00"&gt;&gt;
%% </pre>
-spec unix_seconds_to_rfc3339(unix_seconds()) -> rfc3339().
unix_seconds_to_rfc3339(Seconds) when is_integer(Seconds) ->
    iolist_to_binary(calendar:system_time_to_rfc3339(Seconds, [{unit, second}, {offset, "+09:00"}])).

%% @doc
%% Convert Unix time in nanoseconds to RFC3339 datetime (binary).
%%
%% The returned RFC3339 string uses the fixed offset "+09:00" and includes a
%% nanosecond fractional component (e.g. ".000000000").
%%
%% Example:
%% <pre>
%% 1> klsn_time:unix_nanoseconds_to_rfc3339(0).
%% &lt;&lt;"1970-01-01T09:00:00.000000000+09:00"&gt;&gt;
%% </pre>
-spec unix_nanoseconds_to_rfc3339(unix_nanoseconds()) -> rfc3339().
unix_nanoseconds_to_rfc3339(Nanoseconds) when is_integer(Nanoseconds) ->
    iolist_to_binary(calendar:system_time_to_rfc3339(Nanoseconds, [{unit, nanosecond}, {offset, "+09:00"}])).

%% @doc
%% Convert Unix time from seconds to nanoseconds.
%%
%% This is an exact integer multiplication (1 s = 1,000,000,000 ns).
%%
%% Example:
%% <pre>
%% 1> klsn_time:unix_seconds_to_unix_nanoseconds(-1).
%% -1000000000
%% </pre>
-spec unix_seconds_to_unix_nanoseconds(unix_seconds()) -> unix_nanoseconds().
unix_seconds_to_unix_nanoseconds(Seconds) when is_integer(Seconds) ->
    erlang:convert_time_unit(Seconds, second, nanosecond).

%% @doc
%% Convert Unix time from nanoseconds to seconds.
%%
%% Uses the runtime's floor semantics for negative values
%% (rounding toward negative infinity).
%%
%% Examples:
%% <pre>
%% 1> klsn_time:unix_nanoseconds_to_unix_seconds(1999999999).
%% 1
%% 2> klsn_time:unix_nanoseconds_to_unix_seconds(-1).
%% -1
%% 3> klsn_time:unix_nanoseconds_to_unix_seconds(-1999999999).
%% -2
%% </pre>
-spec unix_nanoseconds_to_unix_seconds(unix_nanoseconds()) -> unix_seconds().
unix_nanoseconds_to_unix_seconds(Nanoseconds) when is_integer(Nanoseconds) ->
    erlang:convert_time_unit(Nanoseconds, nanosecond, second).
