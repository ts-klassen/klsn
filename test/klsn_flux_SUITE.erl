-module(klsn_flux_SUITE).
-include_lib("common_test/include/ct.hrl").

%% Export the suite callback and test cases
-export([all/0,
         main/1,
         protocol_format/1,
         write_empty/1,
         flux_query_empty/1,
         multi_write/1,
         test_value/1,
         test_csv/1,
         test_more_value/1,
         test_timestamp/1,
         test_tags/1,
         test_fields_types/1,
         test_csv_escapes/1,
         test_q/1,
         test_json_query/1]).

%% Define the test suite with all/0
all() ->
    [ main,
      protocol_format,
      write_empty,
      flux_query_empty,
      multi_write,
      test_value,
      test_csv,
      test_more_value,
      test_timestamp,
      test_tags,
      test_fields_types,
      test_csv_escapes,
      test_q,
      test_json_query
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

%% Test the value/1 function covers multiple AST branches
test_value(_Config) ->
    BTrue = klsn_flux:value(true),
    (maps:get(type, BTrue) =:= 'BooleanLiteral' andalso maps:get(value, BTrue) =:= true)
        orelse exit({value_true, BTrue}),
    BFalse = klsn_flux:value(false),
    (maps:get(type, BFalse) =:= 'BooleanLiteral' andalso maps:get(value, BFalse) =:= false)
        orelse exit({value_false, BFalse}),
    TInt = klsn_flux:value(123),
    (maps:get(type, TInt) =:= 'IntegerLiteral' andalso maps:get(value, TInt) =:= 123)
        orelse exit({value_int, TInt}),
    TFloat = klsn_flux:value(1.23),
    (maps:get(type, TFloat) =:= 'FloatLiteral' andalso maps:get(value, TFloat) =:= 1.23)
        orelse exit({value_float, TFloat}),
    TString = klsn_flux:value(<<"abc">>),
    (maps:get(type, TString) =:= 'StringLiteral' andalso maps:get(value, TString) =:= <<"abc">>)
        orelse exit({value_string, TString}),
    TObject = klsn_flux:value({object, #{<<"a">> => 1}}),
    (maps:get(type, TObject) =:= 'ObjectExpression' andalso length(maps:get(properties, TObject)) =:= 1)
        orelse exit({value_object, TObject}),
    TArray = klsn_flux:value({array, [1, <<"b">>]}),
    (maps:get(type, TArray) =:= 'ArrayExpression' andalso length(maps:get(elements, TArray)) =:= 2)
        orelse exit({value_array, TArray}),
    Ts = 1, % small timestamp
    TDate = klsn_flux:value({timestamp, Ts}),
    (maps:get(type, TDate) =:= 'DateTimeLiteral' andalso is_binary(maps:get(values, TDate)))
        orelse exit({value_timestamp, TDate}),
    UDate = klsn_flux:value({unixtime, 1}),
    (maps:get(type, UDate) =:= 'DateTimeLiteral' andalso is_binary(maps:get(values, UDate)))
        orelse exit({value_unixtime, UDate}),
    ok.

%% Test the CSV parser covers basic and quoted fields
test_csv(_Config) ->
    CSV1 = <<"a,b,c\n1,2,3\n">>,
    Rows1 = klsn_flux:csv(CSV1),
    length(Rows1) >= 2 orelse exit({csv_rows, length(Rows1)}),
    [H1 | Rest1] = Rows1,
    R1 = hd(Rest1),
    H1 =:= [<<"a">>,<<"b">>,<<"c">>] orelse exit({csv_hdr, H1}),
    R1 =:= [<<"1">>,<<"2">>,<<"3">>] orelse exit({csv_row, R1}),
    CSV2 = <<"\"col,1\",abc\nx,y,z\n">>,
    Rows2 = klsn_flux:csv(CSV2),
    length(Rows2) >= 2 orelse exit({csv2_rows, length(Rows2)}),
    [H2 | Rest2] = Rows2,
    R2 = hd(Rest2),
    H2 =:= [<<"col,1">>,<<"abc">>] orelse exit({csv_quoted_hdr, H2}),
    R2 =:= [<<"x">>,<<"y">>,<<"z">>] orelse exit({csv_quoted_row, R2}),
    ok.

%% More value/1 branches: unary, call, identifier, regex, map, list, date_time, raw
test_more_value(_Config) ->
    U = klsn_flux:value({unary, <<"-">>, {int, 5}}),
    (maps:get(type, U) =:= 'UnaryExpression' andalso maps:get(operator, U) =:= <<"-">>)
        orelse exit({value_unary, U}),
    C = klsn_flux:value({call, {identifier, foo}}),
    (maps:get(type, C) =:= 'CallExpression') orelse exit({value_call, C}),
    ID = klsn_flux:value({identifier, bar}),
    (maps:get(type, ID) =:= 'Identifier' andalso maps:get(name, ID) =:= bar)
        orelse exit({value_ident, ID}),
    RI = klsn_flux:value(bar),
    (maps:get(type, RI) =:= 'Identifier') orelse exit({value_atom, RI}),
    RX = klsn_flux:value({regex, <<"ab.*">>}),
    (maps:get(type, RX) =:= 'RegexpLiteral') orelse exit({value_regex, RX}),
    MI = klsn_flux:value(#{<<"k">> => 1}),
    (maps:get(type, MI) =:= 'ObjectExpression') orelse exit({value_map, MI}),
    LI = klsn_flux:value([1, <<"a">>]),
    (maps:get(type, LI) =:= 'ArrayExpression') orelse exit({value_list, LI}),
    DT = <<"2020-01-01T00:00:00Z">>,
    DL = klsn_flux:value({date_time, DT}),
    (maps:get(type, DL) =:= 'DateTimeLiteral') orelse exit({value_dt, DL}),
    RM = #{foo => <<"bar">>},
    RR = klsn_flux:value({raw, RM}),
    (RR =:= RM) orelse exit({value_raw, RR}),
    ok.

%% Test timestamp() returns a positive integer
test_timestamp(_Config) ->
    T = klsn_flux:timestamp(),
    (is_integer(T) andalso T > 0) orelse exit({timestamp_bad, T}),
    ok.

%% Test tags are included in line protocol
test_tags(_Config) ->
    Meas = klsn_db:new_id(),
    P = #{measurement => Meas, tag => #{t1 => <<"v1">>}, field => #{f1 => true}},
    LP = klsn_flux:points_to_line_protocol(P),
    binary:match(LP, <<",t1=v1">>) =/= nomatch orelse exit({tag_missing, LP}),
    binary:match(LP, <<" f1=true">>) =/= nomatch orelse exit({field_missing, LP}),
    ok.

%% Test integer and float field formatting
test_fields_types(_Config) ->
    Meas = klsn_db:new_id(),
    P = #{measurement => Meas, field => #{i => 1, f => 1.23}},
    LP = klsn_flux:points_to_line_protocol(P),
    binary:match(LP, <<"i=1i">>) =/= nomatch orelse exit({int_missing, LP}),
    %% float should appear with f= prefix
    binary:match(LP, <<"f=">>) =/= nomatch orelse exit({float_missing, LP}),
    ok.

%% Test CSV parser handles escaped quotes in quoted fields
test_csv_escapes(_Config) ->
    CSV3 = <<"\"a\\\"b\",c\r\n">>,
    Rows3 = klsn_flux:csv(CSV3),
    length(Rows3) >= 1 orelse exit({csv3_rows, length(Rows3)}),
    [H3|_] = Rows3,
    H3 =:= [<<"a\"b">>,<<"c">>] orelse exit({csv3_hdr, H3}),
    ok.

%% Test q/3 returns a list of maps for a written point
test_q(_Config) ->
    Org = default, Bucket = sandbox,
    Meas = klsn_db:new_id(),
    Ts = klsn_flux:timestamp(),
    P = #{measurement => Meas, field => #{test => 123}, timestamp => Ts},
    ok = klsn_flux:write(Org, Bucket, P),
    Flux = iolist_to_binary(io_lib:format(
               "from(bucket: \"~s\") |> range(start: -1m)"
               ++
               " |> filter(fn: (r) => r._measurement == \"~s\" and r._field == \"test\")",
               [atom_to_list(Bucket), binary_to_list(Meas)])),
    QRes = klsn_flux:q(Org, Flux, {object, #{}}),
    length(QRes) >= 1 orelse exit({q_empty, QRes}),
    Map = hd(QRes),
    maps:get(<<"_field">>, Map) =:= <<"test">> orelse exit({q_field, Map}),
    ValueBin = maps:get(<<"_value">>, Map),
    binary:match(ValueBin, <<"123">>) =/= nomatch orelse exit({q_value, ValueBin}),
    ok.
%% Test flux_query/3 JSON branch returns binary
test_json_query(_Config) ->
    QMap = #{<<"query">> => <<"from(bucket:\"sandbox\") |> range(start:-1m)">>},
    Res = klsn_flux:flux_query(default, QMap),
    is_binary(Res) orelse exit({json_query_fail, Res}),
    ok.

