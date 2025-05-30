-module(klsn_flux_SUITE).
-include_lib("common_test/include/ct.hrl").

%% Export the suite callback and test cases
-export([all/0,
         main/1]).

%% Define the test suite with all/0
all() ->
    [
        main
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

