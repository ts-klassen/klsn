-module(klsn_flux_SUITE).
-include_lib("common_test/include/ct.hrl").

%% Export the suite callback and test cases
-export([all/0,
         main/1,
         protocol_format/1,
         write_empty/1,
         flux_query_empty/1,
         multi_write/1]).

%% Define the test suite with all/0
all() ->
    [ main,
      protocol_format,
      write_empty,
      flux_query_empty,
      multi_write
    ].

%% Just make sure there are no errors

main(_Config) ->
    Org = default,
    Bucket = sandbox,
    Measurement = klsn_db:new_id(),
    Point = #{
        measurement => Measurement,
        field => #{
            test => 1
        }
    },
    ok = klsn_flux:write(Org, Bucket, Point),
    %% Verify data was persisted by querying InfluxDB
    Flux = iolist_to_binary(
               io_lib:format(
                 "from(bucket: \"~s\") |> range(start: -1m)"
                 ++
                 " |> filter(fn: (r) => r._measurement == \"~s\" and r._field == \"test\")",
                 [atom_to_list(Bucket), binary_to_list(Measurement)])),
    CSV = klsn_flux:flux_query(Org, Flux),
    [Header | Rows] = klsn_flux:csv(CSV),
    Cols = lists:zip(Header, lists:seq(1, length(Header))),
    {_, MeasIdx} = lists:keyfind(<<"_measurement">>, 1, Cols),
    {_, ValueIdx} = lists:keyfind(<<"_value">>, 1, Cols),
    Row = hd(Rows),
    MeasRaw = lists:nth(MeasIdx, Row),
    MeasRaw =:= Measurement orelse exit({measurement_mismatch, MeasRaw, Measurement}),
    ValueRaw = lists:nth(ValueIdx, Row),
    case ValueRaw of
        <<"1">>   -> ok;
        <<"1.0">> -> ok
    end,
    ok.

%% Test that line protocol output has the correct prefix for a point
protocol_format(_Config) ->
    Meas = klsn_db:new_id(),
    Point = #{measurement => Meas, field => #{test => 1}},
    LP = klsn_flux:points_to_line_protocol(Point),
    Prefix = iolist_to_binary([klsn_binstr:from_any(Meas), " test=1i"]),
    case binary:match(LP, Prefix) of
        {0,_} -> ok;
        _ -> exit({protocol_format_mismatch, LP})
    end.

%% Test writing multiple points in one call and retrieving both
multi_write(_Config) ->
    Org = default, Bucket = sandbox,
    Meas = klsn_db:new_id(),
    Ts0 = klsn_flux:timestamp(),
    Ts1 = Ts0,
    Ts2 = Ts0 + 1,
    P1 = #{measurement => Meas, field => #{test => 10}, timestamp => Ts1},
    P2 = #{measurement => Meas, field => #{test => 20}, timestamp => Ts2},
    ok = klsn_flux:write(Org, Bucket, [P1, P2]),
    Flux = iolist_to_binary(
             io_lib:format(
               "from(bucket: \"~s\") |> range(start: -1m) |> filter(fn: (r) => r._measurement == \"~s\" and r._field == \"test\")",
               [atom_to_list(Bucket), binary_to_list(Meas)])),
    CSV = klsn_flux:flux_query(Org, Flux),
    [Header | Rows0] = klsn_flux:csv(CSV),
    Cols = lists:zip(Header, lists:seq(1, length(Header))),
    {_, MeasIdx} = lists:keyfind(<<"_measurement">>, 1, Cols),
    HeaderLen = length(Header),
    ActualRows = [Row || Row <- Rows0,
                       length(Row) =:= HeaderLen,
                       lists:nth(MeasIdx, Row) =:= Meas],
    length(ActualRows) =:= 2 orelse exit({expected_two_rows, length(ActualRows)}),
    ok.

%% Test writing an empty list of points returns ok
write_empty(_Config) ->
    ok = klsn_flux:write(default, sandbox, []),
    ok.

%% Test flux_query with an empty query returns ok
flux_query_empty(_Config) ->
    ok = klsn_flux:flux_query(default, []),
    ok.

