-module(klsn_flux).

-export([
        write/3
      , write/4
      , flux_query/2
      , flux_query/3
      , q/3
      , q/4
      , value/1
      , timestamp/0
      , points_to_line_protocol/1
      , csv/1
    ]).
-export_type([
        info/0
      , key/0
      , field_value/0
      , timestamp/0
      , unixtime/0
      , date_time/0
      , organization/0
      , bucket/0
    ]).

%% Connection details for the InfluxDB HTTP API. Built by info/0 when the
%% caller does not supply its own map.
-type info() :: #{
        uri_map := unicode:unicode_binary()
      , headers := [{[], []}]
    }.
%% Identifier that can appear in measurement, tag, field or as an AST
%% element when building Flux queries.
-type key() :: atom() | klsn:binstr().
%% Allowed value inside the field map of a point.
-type field_value() :: key()
                     | integer()
                     | float()
                     | boolean()
                     .
%% Unix time in nanoseconds since epoch.
-type timestamp() :: integer(). % nanosecond
%% Unix time in seconds since epoch.
-type unixtime() :: integer(). % second
%% RFC-3339 timestamp as UTF-8 binary.
-type date_time() :: klsn:binstr(). % rfc-3339
-type point() :: #{
        measurement := key()
      , tag => maps:map(key(), key())
      , field := maps:map(key(), field_value())
      , timestamp => timestamp()
    }.
%% InfluxDB organisation.
-type organization() :: key().
%% InfluxDB bucket name.
-type bucket() :: key().
-type unit() :: d | h | m | s.
-type value() ::
    {object, maps:map(Identifier::value(), Value::value())}
  | Object::maps:map(Identifier::value(), Value::value())
  | {array, Array::[Element::value()]}
  | Array::[Element::value()]
  | {unary, Operator::klsn:binstr(), Value::value()}
  | {call, value()}
  | {bool, Bool::boolean()} | Bool::boolean()
  | {identifier, Name::key()} | Identifier::atom()
  | {int, Int::integer()} | Int::integer()
  | {uint, UInt::non_neg_integer()}
  | {uint, Float::float()} | Float::float()
  | {string, String::klsn:binstr()} | String::klsn:binstr()
  | {duration, [{Magnitude::integer(), unit()}]}
  | {date_time, DateTime::date_time()}
  | {timestamp, NanoSecond::timestamp()}
  | {unixtime, Second::unixtime()}
  | {regex, Regex::klsn:binstr()}
  | {raw, #{}}
  .

-spec post(#{
        q := #{}
      , path := klsn:binstr()
      , ctype := []
      , body := klsn:binstr()
    }, info()) -> klsn:binstr().
post(Request, #{uri_map:=UriMap, headers:=Headers}) ->
    #{
        q := Query
      , path := Path
      , ctype := CType
      , body := Body
    } = Request,
    QueryStr = uri_string:compose_query(maps:to_list(Query)),
    Url = uri_string:recompose(UriMap#{
        'query' => QueryStr
      , path => Path
    }),
    Res = httpc:request(post, {Url, Headers, CType, Body}, [], [{body_format, binary}]),
    case Res of
        {ok, {{_, Stat, _}, _, Data}} when 200=<Stat,Stat=<299 ->
            Data;
        {ok, {{_, Stat, _}, _, Data}} ->
            error({klsn_flux_status_error, Stat, Data});
        {error, Error} ->
            error({klsn_flux_httpc_error, Error})
    end.


%% @doc
%% Write one or more Points to Bucket (within Org) using the InfluxDB
%% /api/v2/write endpoint. Automatically retries on transient errors.
-spec write(
        organization()
      , bucket()
      , point() | [point()]
    ) -> ok.
write(Org, Bucket, Points) ->
    write(Org, Bucket, Points, info()).

%% @doc
%% Same as write/3 but with explicit connection Info.
-spec write(
        organization()
      , bucket()
      , point() | [point()]
      , info()
    ) -> ok.
write(_Org, _Bucket, [], _Info) ->
    ok;
write(Org, Bucket, Points, Info) ->
    write_(Org, Bucket, points_to_line_protocol(Points), Info, 1).

write_(Org, Bucket, Body, Info, Retry) ->
    try
        post(#{
            q => #{
                <<"org">> => klsn_binstr:from_any(Org)
              , <<"bucket">> => klsn_binstr:from_any(Bucket)
            }
          , path => <<"/api/v2/write">>
          , ctype => ""
          , body => Body
        }, Info)
    of
        <<>> ->
            ok
    catch
        error:Error={klsn_flux_status_error, 400, _}:Stack ->
            erlang:raise(error,Error,Stack);
        error:Error={klsn_flux_status_error, 422, _}:Stack ->
            erlang:raise(error,Error,Stack);
        Class:Error:Stack ->
            spawn(fun()-> erlang:raise(Class,Error,Stack) end),
            sleep(Retry, 10, {Class,Error,Stack}),
            write_(Org, Bucket, Body, Info, Retry+1)
    end.


%% @doc
%% Convenience helper that parameterises a Flux Query with Args, sends
%% it via flux_query/2 and returns the first table as a list of maps
%% (header row is stripped).
-spec q(
        organization()
      , Query::klsn:binstr()
      , Args::value()
    ) -> maps:map(klsn:binstr(), klsn:binstr()).
q(Org, Query, Args) ->
    q(Org, Query, Args, info()).


%% @doc
%% Same as q/3 but lets the caller specify Info (target server, auth headers…).
-spec q(
        organization()
      , Query::klsn:binstr()
      , Args::value()
      , info()
    ) -> maps:map(klsn:binstr(), klsn:binstr()).
q(Org, Query, Args, Info) ->
    CSV = flux_query(Org, #{
        'query' => Query
      , extern => #{
            type => 'File'
          , package => null
          , imports => null
          , body => [#{
                type => 'OptionStatement'
              , assignment => #{
                    type => 'VariableAssignment'
                  , id => value(args)
                  , init => value(Args)
                }
            }]
        }
    }, Info),
    [Header|Body] = csv(CSV),
    HeaderLength = length(Header),
    lists:filtermap(fun(Row)->
        case length(Row) of
            Len when Len =:= HeaderLength ->
                {true, maps:from_list(lists:zip(tl(Header), tl(Row)))};
            _ ->
                false
        end
    end, Body).


%% @doc
%% Send a raw Flux script (binary) or JSON query object to InfluxDB and
%% return the server response (CSV) as a binary.
-spec flux_query(
        organization()
      , klsn:binstr() | #{}
    ) -> ok.
flux_query(Org, Query) ->
    flux_query(Org, Query, info()).

%% @doc
%% Version of flux_query/2 that takes custom connection Info.
-spec flux_query(
        organization()
      , klsn:binstr() | #{}
      , info()
    ) -> ok.
flux_query(_Org, [], _Info) ->
    ok;
flux_query(Org, Query, Info) ->
    flux_query_(Org, Query, Info, 1).

flux_query_(Org, Query, Info, Retry) ->
    try
        post(#{
            q => #{
                <<"org">> => klsn_binstr:from_any(Org)
            }
          , path => <<"/api/v2/query">>
          , ctype => case Query of
                #{} ->
                    "application/json";
                _ ->
                    "application/vnd.flux"
            end
          , body => case Query of
                #{} ->
                    jsone:encode(Query);
                _ ->
                    Query
            end
        }, Info)
    of
        Res ->
            Res
    catch
        error:Error={klsn_flux_status_error, 400, _}:Stack ->
            erlang:raise(error,Error,Stack);
        Class:Error:Stack ->
            spawn(fun()-> erlang:raise(Class,Error,Stack) end),
            sleep(Retry, 10, {Class,Error,Stack}),
            flux_query_(Org, Query, Info, Retry+1)
    end.


%% @doc
%% Convert a *Point* or list of points to InfluxDB Line Protocol. Tags are
%% alphabetically stable and strings are properly escaped.
-spec points_to_line_protocol(point() | [point()]) -> klsn:binstr().
points_to_line_protocol(Point) when is_map(Point) ->
    points_to_line_protocol([Point]);
points_to_line_protocol(Points) ->
    TimestampNow = timestamp(),
    iolist_to_binary(lists:map(fun
        (Point=#{
            measurement := Measurement
          , field := FieldMap
        }) ->
            TagMap = maps:get(tag, Point, #{}),
            Timestamp = case Point of
                #{timestamp := Timestamp0} -> Timestamp0;
                _ -> TimestampNow
            end,
            [
                klsn_binstr:from_any(Measurement)
              , lists:map(fun({Key, Val})->
                    [
                        $,
                      , klsn_binstr:from_any(Key)
                      , $=
                      , klsn_binstr:from_any(Val)
                    ]
                end, maps:to_list(TagMap))
              , $\s
              , tl(lists:flatten(lists:map(fun({Key, Val})->
                    [
                        $,
                      , klsn_binstr:from_any(Key)
                      , $=
                      , case Val of
                            true ->
                                <<"true">>;
                            false ->
                                <<"false">>;
                            null ->
                                <<"null">>;
                            Int when is_integer(Int) ->
                                [klsn_binstr:from_any(Val), $i];
                            Float when is_number(Float) ->
                                klsn_binstr:from_any(Val);
                            _ ->
                                [
                                    $"
                                  , klsn_binstr:replace(
                                        [   {<<"\\">>, <<"\\\\">>}
                                          , {<<"\n">>, <<"\\n">>}
                                          , {<<"\r">>, <<"\\r">>}
                                          , {<<"\t">>, <<"\\t">>}
                                          , {<<"\"">>, <<"\\\"">>}]
                                      , klsn_binstr:from_any(Val)
                                    )
                                  , $"
                                ]
                        end
                    ]
                end, maps:to_list(FieldMap))))
              , $\s
              , klsn_binstr:from_any(Timestamp)
              , $\n
            ]
    end, Points)).

%% @doc
%% Current Unix time in nanoseconds, suitable for line protocol points
%% that omit an explicit timestamp.
-spec timestamp() -> timestamp().
timestamp() ->
    os:system_time(nanosecond).

info() ->
    Url = case os:getenv("INFLUXDB_URL") of
        false ->
            <<"http://localhost:8086">>;
        Str when is_list(Str) ->
            iolist_to_binary(Str)
    end,
    Headers = case os:getenv("INFLUXDB_TOKEN") of
        false ->
            [];
        TokenStr when is_list(TokenStr) ->
            [{"Authorization", "Token " ++ TokenStr}]
    end,
    #{uri_map => uri_string:parse(Url), headers => Headers}.

sleep(Stage, TooMany, {Class,Error,Stack}) when Stage >= TooMany ->
    erlang:raise(Class,Error,Stack);
sleep(Stage, _, _) ->
    timer:sleep(round(1000 * rand:uniform() + 100 * math:exp(Stage))).


%% @doc
%% Convert the simplified Erlang representation returned by klsn_flux:q/3
%% (or hand-crafted by callers) into the full JSON AST expected by the
%% InfluxDB query API.

value({object, Properties}) ->
    #{
        type => 'ObjectExpression'
      , properties => lists:map(fun({Key, Value})->
            #{
                type => 'Property'
              , key => value(Key)
              , value => value(Value)
            }
        end, maps:to_list(Properties))
    };
value({array, Elements}) ->
    #{
        type => 'ArrayExpression'
      , elements => lists:map(fun(Value)->
            value(Value)
        end, Elements)
    };
value({unary, Operator, Value}) ->
    #{
        type => 'UnaryExpression'
      , operator => Operator
      , argument => value(Value)
    };
value({call, Value}) ->
    #{
        type => 'CallExpression'
      , callee => value(Value)
    };
value({bool, Bool}) ->
    #{
        type => 'BooleanLiteral'
      , value => Bool
    };
value({identifier, Identifier}) ->
    #{
        type => 'Identifier'
      , name => Identifier
    };
value({int, Int}) ->
    #{
        type => 'IntegerLiteral'
      , value => Int
    };
value({uint, UInt}) ->
    #{
        type => 'UnsignedIntegerLiteral'
      , value => UInt
    };
value({float, Float}) ->
    #{
        type => 'FloatLiteral'
      , value => Float
    };
value({string, String}) ->
    #{
        type => 'StringLiteral'
      , value => String
    };
value({duration, Args}) ->
    #{
        type => 'DurationLiteral'
      , values => lists:map(fun({Magnitude, Unit})->
            #{
                magnitude => Magnitude
              , unit => Unit
            }
        end, Args)
    };
value({date_time, DateTime}) ->
    #{
        type => 'DateTimeLiteral'
      , values => DateTime
    };
value({regex, Regex}) ->
    #{
        type => 'RegexpLiteral'
      , values => Regex
    };
value({raw, Raw}) ->
    Raw;

value(true) ->
    value({bool, true});
value(false) ->
    value({bool, false});
value(Identifier) when is_atom(Identifier) ->
    value({identifier, Identifier});
value(Int) when is_integer(Int) ->
    value({int, Int});
value(Float) when is_float(Float) ->
    value({float, Float});
value(String) when is_binary(String) ->
    value({string, String});
value(Object) when is_map(Object) ->
    value({object, Object});
value(Elements) when is_list(Elements) ->
    value({array, Elements});
value({timestamp, Timestamp}) ->
    value({date_time, iolist_to_binary(calendar:system_time_to_rfc3339(Timestamp, [{unit, nanosecond}]))});
value({unixtime, Unixtime}) ->
    value({date_time, iolist_to_binary(calendar:system_time_to_rfc3339(Unixtime, [{unit, second}]))});

value(Arg) ->
    erlang:error(badarg, [Arg]).


%% @doc
%% Very small CSV parser used by q/3,4. Returns the data as a list of
%% rows where each cell is a UTF-8 binary.
-spec csv(klsn:binstr()) -> [[klsn:binstr()]].
csv(CSV) ->
    csv(CSV, normal, [[<<>>]]).

-spec csv(
        klsn:binstr()
      , normal | quote
      , [[klsn:binstr()]]
    ) -> [[klsn:binstr()]].
csv(<<>>, _, [Row|Res]) ->
    lists:reverse([lists:reverse(Row)|Res]);
csv(<<"\\", C:1/binary, Tail/binary>>, State, [[Bin|Row]|Res]) ->
    E = case C of
        $\\ -> $\\;
        $n -> $\n;
        $r -> $\r;
        $t -> $\t;
        _ -> C
    end,
    csv(Tail, State, [[<<Bin/binary, E:1/binary>>|Row]|Res]);
csv(<<"\r\n", Tail/binary>>, normal, [Row|Res]) ->
    csv(Tail, normal, [[<<>>], lists:reverse(Row) | Res]);
csv(<<"\n", Tail/binary>>, normal, [Row|Res]) ->
    csv(Tail, normal, [[<<>>], lists:reverse(Row) | Res]);
csv(<<",", Tail/binary>>, normal, [Row|Res]) ->
    csv(Tail, normal, [[<<>>|Row]|Res]);
csv(<<"\"", Tail/binary>>, normal, Res) ->
    csv(Tail, quote, Res);
csv(<<"\"", Tail/binary>>, quote, Res) ->
    csv(Tail, normal, Res);
csv(<<C:1/binary, Tail/binary>>, State, [[Bin|Row]|Res]) ->
    csv(Tail, State, [[<<Bin/binary, C:1/binary>>|Row]|Res]).

