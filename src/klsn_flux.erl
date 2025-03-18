-module(klsn_flux).

-export([
        write/3
      , write/4
%     , flux_query/3
%     , flux_query/4
      , timestamp/0
      , points_to_line_protocol/1
    ]).
-export_type([
        info/0
      , key/0
      , field_value/0
      , timestamp/0
      , organization/0
      , bucket/0
    ]).

-type info() :: #{
        uri_map := unicode:unicode_binary()
      , headers := [{[], []}]
    }.
-type key() :: atom() | klsn:binstr().
-type field_value() :: key()
                     | integer()
                     | float()
                     | boolean()
                     .
-type timestamp() :: integer(). % nanosecond
-type point() :: #{
        measurement := key()
      , tag => maps:map(key(), key())
      , field := maps:map(key(), field_value())
      , timestamp => timestamp()
    }.
-type organization() :: key().
-type bucket() :: key().

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
        {ok, {{_, Stat, _}, _, _}} ->
            error({klsn_flux_status_error, Stat});
        {error, Error} ->
            error({klsn_flux_httpc_error, Error})
    end.


-spec write(
        organization()
      , bucket()
      , point() | [point()]
    ) -> ok.
write(Org, Bucket, Points) ->
    write(Org, Bucket, Points, info()).

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

write_(_Org, _Bucket, _Body, _Info, ReTry) when ReTry >= 10 ->
    error(too_many_retry);
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
        error:{klsn_flux_status_error, 500} ->
            sleep(Retry),
            write_(Org, Bucket, Body, Info, Retry+1);
        Class:Error:Stack ->
            spawn(fun()-> erlang:raise(Class,Error,Stack) end),
            sleep(Retry),
            write_(Org, Bucket, Body, Info, Retry+5)
    end.


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

sleep(Stage) ->
    timer:sleep(round(1000 * rand:uniform() + 100 * math:exp(Stage))).


