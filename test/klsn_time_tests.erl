-module(klsn_time_tests).

-include_lib("eunit/include/eunit.hrl").

%% rfc3339_to_unix_seconds/1
rfc3339_to_unix_seconds_test() ->
    ?assertEqual(0, klsn_time:rfc3339_to_unix_seconds(<<"1970-01-01T00:00:00Z">>)),
    ?assertEqual(0, klsn_time:rfc3339_to_unix_seconds(<<"1970-01-01T09:00:00+09:00">>)),
    ?assertEqual(0, klsn_time:rfc3339_to_unix_seconds(<<"1969-12-31T15:00:00-09:00">>)),
    ?assertEqual(1000000000, klsn_time:rfc3339_to_unix_seconds(<<"2001-09-09T01:46:40Z">>)),
    ?assertEqual(-1, klsn_time:rfc3339_to_unix_seconds(<<"1969-12-31T23:59:59Z">>)),
    ok.

%% rfc3339_to_unix_nanoseconds/1
rfc3339_to_unix_nanoseconds_test() ->
    ?assertEqual(0, klsn_time:rfc3339_to_unix_nanoseconds(<<"1970-01-01T00:00:00Z">>)),
    ?assertEqual(123456789, klsn_time:rfc3339_to_unix_nanoseconds(<<"1970-01-01T00:00:00.123456789Z">>)),
    ?assertEqual(-1500000000, klsn_time:rfc3339_to_unix_nanoseconds(<<"1969-12-31T23:59:59.500000000Z">>)),
    %% +09:00 offset equivalents
    ?assertEqual(0, klsn_time:rfc3339_to_unix_nanoseconds(<<"1970-01-01T09:00:00+09:00">>)),
    ?assertEqual(123456789, klsn_time:rfc3339_to_unix_nanoseconds(<<"1970-01-01T09:00:00.123456789+09:00">>)),
    ?assertEqual(-1500000000, klsn_time:rfc3339_to_unix_nanoseconds(<<"1970-01-01T08:59:59.500000000+09:00">>)),
    %% A later timestamp (1,000,000,000 seconds since epoch)
    ?assertEqual(1000000000000000000, klsn_time:rfc3339_to_unix_nanoseconds(<<"2001-09-09T10:46:40+09:00">>)),
    ?assertEqual(1000000000123456789, klsn_time:rfc3339_to_unix_nanoseconds(<<"2001-09-09T10:46:40.123456789+09:00">>)),
    ok.

%% unix_seconds_to_rfc3339/1 (offset +09:00)
unix_seconds_to_rfc3339_test() ->
    ?assertEqual(<<"1970-01-01T09:00:00+09:00">>, klsn_time:unix_seconds_to_rfc3339(0)),
    ?assertEqual(<<"2001-09-09T10:46:40+09:00">>, klsn_time:unix_seconds_to_rfc3339(1000000000)),
    ?assertEqual(<<"1970-01-01T08:59:59+09:00">>, klsn_time:unix_seconds_to_rfc3339(-1)),
    ok.

%% unix_nanoseconds_to_rfc3339/1 (offset +09:00)
unix_nanoseconds_to_rfc3339_test() ->
    ?assertEqual(<<"1970-01-01T09:00:00.000000000+09:00">>, klsn_time:unix_nanoseconds_to_rfc3339(0)),
    ?assertEqual(<<"1970-01-01T09:00:00.123456789+09:00">>, klsn_time:unix_nanoseconds_to_rfc3339(123456789)),
    ?assertEqual(<<"1970-01-01T08:59:59.500000000+09:00">>, klsn_time:unix_nanoseconds_to_rfc3339(-500000000)),
    ?assertEqual(<<"1970-01-01T08:59:59.999999999+09:00">>, klsn_time:unix_nanoseconds_to_rfc3339(-1)),
    ok.

%% unix_seconds_to_unix_nanoseconds/1
unix_seconds_to_unix_nanoseconds_test() ->
    ?assertEqual(0, klsn_time:unix_seconds_to_unix_nanoseconds(0)),
    ?assertEqual(1000000000, klsn_time:unix_seconds_to_unix_nanoseconds(1)),
    ?assertEqual(-1000000000, klsn_time:unix_seconds_to_unix_nanoseconds(-1)),
    ok.

%% unix_nanoseconds_to_unix_seconds/1 (truncates toward zero)
unix_nanoseconds_to_unix_seconds_test() ->
    ?assertEqual(0, klsn_time:unix_nanoseconds_to_unix_seconds(0)),
    ?assertEqual(1, klsn_time:unix_nanoseconds_to_unix_seconds(1999999999)),
    ?assertEqual(0, klsn_time:unix_nanoseconds_to_unix_seconds(999999999)),
    ?assertEqual(-2, klsn_time:unix_nanoseconds_to_unix_seconds(-1999999999)),
    ?assertEqual(-1, klsn_time:unix_nanoseconds_to_unix_seconds(-1)),
    ok.
