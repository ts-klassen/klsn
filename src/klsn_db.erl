-module(klsn_db).

-export([
        create_db/1
      , create_db/2
      , create_doc/2
      , create_doc/3
      , bulk_create_doc/2
      , bulk_create_doc/3
      , get/2
      , get/3
      , lookup/2
      , lookup/3
      , bulk_lookup/2
      , bulk_lookup/3
      , mango_find/2
      , mango_find/3
      , mango_index/2
      , mango_index/3
      , mango_explain/2
      , mango_explain/3
      , update/3
      , update/4
      , upsert/3
      , upsert/4
      , bulk_upsert/3
      , bulk_upsert/4
      , time_now/0
      , new_id/0
      , db_info/0
    ]).
-export_type([
        info/0
      , db/0
      , key/0
      , payload/0
      , value/0
      , id/0
      , rev/0
      , update_function/0
      , upsert_function/0
    ]).

%% ------------------------------------------------------------------
%% Exported types
%% ------------------------------------------------------------------

%% Connection information used by the helper functions when talking to a
%% CouchDB-compatible server. Currently only the base URL is recorded.
-type info() :: #{
        url := unicode:unicode_binary()
    }.

%% Name of the database (will be url-encoded when used in a request).
-type db() :: unicode:unicode_binary().

%% Document key (i.e. the _id field) inside the database.
-type key() :: unicode:unicode_binary().

%% Document identifier returned by CouchDB after a create / update.
-type id() :: unicode:unicode_binary().

%% Revision string returned by CouchDB (the _rev field).
-type rev() :: unicode:unicode_binary().

%% JSON-serialisable map that becomes the body of a CouchDB document.
-type payload() :: maps:map(atom() | unicode:unicode_binary(), value()).

%% Allowed JSON values used inside a payload().
-type value() :: atom()
               | unicode:unicode_binary()
               | lists:list(value())
               | maps:map(atom() | unicode:unicode_binary(), value())
               .

%% Callback used by update/3,4. Receives the existing payload() and
%% must return the updated one.
-type update_function() :: fun((payload())->payload()).

%% Callback used by upsert/3,4. Receives none when the document is
%% missing, or {value, Payload} when it exists, and must return the new
%% version that will be stored.
-type upsert_function() :: fun((klsn:'maybe'(payload()))->payload()).

%% @doc
%% Create a new database named *Db* on the configured CouchDB server. If
%% the database already exists the call is idempotent and still returns
%% ok.
-spec create_db(db()) -> ok.
create_db(Db) ->
    create_db(Db, db_info()).

%% @doc
%% Same as create_db/1 but allows passing a custom connection Info
%% record (usually produced by db_info/0).
-spec create_db(db(), info()) -> ok.
create_db(Db, Info) when is_atom(Db) ->
    create_db(atom_to_binary(Db), Info);
create_db(Db0, #{url:=Url0}) ->
    Db1 = klsn_binstr:urlencode(Db0),
    Db = <<"/", Db1/binary>>,
    Url = <<Url0/binary, Db/binary>>,
    Res = httpc:request(put, {Url, []}, [], [{body_format, binary}]),
    case Res of
        {ok, {{_, Stat, _}, _, _}} when 200=<Stat,Stat=<299 ->
            ok;
        {ok, {{_, 412, _}, _, _}} ->
            error(exists)
    end.


%% @doc
%% Run a Mango query against Db using the provided Body.
%%
%% Body must be a JSON-serialisable map compatible with CouchDB's
%% /_find endpoint. Returns the list of matching documents from the
%% "docs" field.
-spec mango_find(db(), map()) -> [payload()].
mango_find(Db, Body) ->
    mango_find(Db, Body, db_info()).

-spec mango_find(db(), map(), info()) -> [payload()].
mango_find(Db, Body, Info) when is_atom(Db) ->
    mango_find(atom_to_binary(Db), Body, Info);
mango_find(Db0, Body0, #{url := Url0}) ->
    Db1 = klsn_binstr:urlencode(Db0),
    Path = <<"/", Db1/binary, "/_find">>,
    Url = <<Url0/binary, Path/binary>>,
    Payload = jsone:encode(Body0),
    Res = httpc:request(post, {Url, [], "application/json", Payload}, [], [{body_format, binary}]),
    case Res of
        {ok, {{_, Stat, _}, _, Data}} when 200 =< Stat, Stat =< 299 ->
            #{<<"docs">> := Docs} = jsone:decode(Data),
            Docs;
        {ok, {{_, 404, _}, _, _}} ->
            error(not_found)
    end.


%% @doc
%% Create a Mango index on Db using the provided Body.
%%
%% Body must be a JSON-serialisable map for the /_index endpoint.
%% Returns the decoded response (e.g. #{"result" := "created"|"exists", ...}).
-spec mango_index(db(), map()) -> map().
mango_index(Db, Body) ->
    mango_index(Db, Body, db_info()).

-spec mango_index(db(), map(), info()) -> map().
mango_index(Db, Body, Info) when is_atom(Db) ->
    mango_index(atom_to_binary(Db), Body, Info);
mango_index(Db0, Body0, #{url := Url0}) ->
    Db1 = klsn_binstr:urlencode(Db0),
    Path = <<"/", Db1/binary, "/_index">>,
    Url = <<Url0/binary, Path/binary>>,
    Payload = jsone:encode(Body0),
    Res = httpc:request(post, {Url, [], "application/json", Payload}, [], [{body_format, binary}]),
    case Res of
        {ok, {{_, Stat, _}, _, Data}} when 200 =< Stat, Stat =< 299 ->
            jsone:decode(Data);
        {ok, {{_, 404, _}, _, _}} ->
            error(not_found)
    end.


%% @doc
%% Explain a Mango query plan for Db using the provided Body.
%%
%% Body matches the /_find request body. Returns the decoded explanation map.
-spec mango_explain(db(), map()) -> map().
mango_explain(Db, Body) ->
    mango_explain(Db, Body, db_info()).

-spec mango_explain(db(), map(), info()) -> map().
mango_explain(Db, Body, Info) when is_atom(Db) ->
    mango_explain(atom_to_binary(Db), Body, Info);
mango_explain(Db0, Body0, #{url := Url0}) ->
    Db1 = klsn_binstr:urlencode(Db0),
    Path = <<"/", Db1/binary, "/_explain">>,
    Url = <<Url0/binary, Path/binary>>,
    Payload = jsone:encode(Body0),
    Res = httpc:request(post, {Url, [], "application/json", Payload}, [], [{body_format, binary}]),
    case Res of
        {ok, {{_, Stat, _}, _, Data}} when 200 =< Stat, Stat =< 299 ->
            jsone:decode(Data);
        {ok, {{_, 404, _}, _, _}} ->
            error(not_found)
    end.


%% @doc
%% Insert a new document Data into Db and return the {Id, Rev} pair
%% assigned by the server. Convenience wrapper that uses default *Info*.
-spec create_doc(db(), payload()) -> {id(), rev()}.
create_doc(Db, Data0) ->
    create_doc(Db, Data0, db_info()).

%% @doc
%% Same as create_doc/2 but with explicit Info.
-spec create_doc(db(), payload(), info()) -> {id(), rev()}.
create_doc(Db, Data0, Info) ->
    Data2 = remove_keys(['_rev', 'C', 'U'], Data0),
    TimeNow = time_now(),
    Data = Data2#{<<"U">>=>TimeNow, <<"C">>=>TimeNow},
    post(Db, Data, Info).

-spec bulk_create_doc(db(), [payload()]) -> [klsn:'maybe'({id(), rev()})].
bulk_create_doc(Db, Docs) ->
    bulk_create_doc(Db, Docs, db_info()).
-spec bulk_create_doc(db(), [payload()], info()) -> [klsn:'maybe'({id(), rev()})].
bulk_create_doc(_Db, [], _Info) -> [];
bulk_create_doc(Db, Docs0, #{url := Url0}) when is_list(Docs0) ->
    TimeNow = time_now(),
    Docs1 = lists:map(
        fun(D0) ->
            D1 = remove_keys(['_rev', 'C', 'U'], D0),
            D1#{<<"U">> => TimeNow, <<"C">> => TimeNow}
        end,
        Docs0),
    DbBin = klsn_binstr:urlencode(klsn_binstr:from_any(Db)),
    Path = <<"/", DbBin/binary, "/_bulk_docs">>,
    Url = <<Url0/binary, Path/binary>>,
    Body = jsone:encode(#{<<"docs">> => Docs1}),
    Res = httpc:request(post, {Url, [], "application/json", Body}, [], [{body_format, binary}]),
    case Res of
        {ok, {{_, Stat, _}, _, Data}} when 200 =< Stat, Stat =< 299 ->
            Results = jsone:decode(Data),
            lists:map(
                fun(#{<<"ok">> := true, <<"id">> := Id, <<"rev">> := Rev}) -> {value, {Id, Rev}};
                   (_) -> none
                end,
                Results);
        _ ->
            lists:duplicate(length(Docs0), none)
    end.

%% @doc
%% Fetch the document identified by Key from Db or raise error:not_found.
-spec get(db(), key()) -> payload().
get(Db, Key) ->
    get(Db, Key, db_info()).

%% @doc
%% Same as get/2 but with explicit Info.
-spec get(db(), key(), info()) -> payload().
get(Db, Key, Info) ->
    case lookup(Db, Key, Info) of
        {value, Value} -> Value;
        none -> error(not_found)
    end.

%% @doc
%% Safe variant of get/2. Returns {value, Payload} when the document
%% exists or none when it is missing.
-spec lookup(db(), key()) -> klsn:'maybe'(payload()).
lookup(Db, Key) ->
    lookup(Db, Key, db_info()).

%% @doc
%% Same as lookup/2 but with explicit Info.
-spec lookup(db(), key(), info()) -> klsn:'maybe'(payload()).
lookup(Db, Key, Info) when is_atom(Db) ->
    lookup(atom_to_binary(Db), Key, Info);
lookup(_, <<>>, _) ->
    none;
lookup(Db0, {raw, Key0}, #{url:=Url0}) -> % for _design view
    Db1 = klsn_binstr:urlencode(Db0),
    Db = <<"/", Db1/binary>>,
    Key = <<"/", Key0/binary>>,
    Url = <<Url0/binary, Db/binary, Key/binary>>,
    Res = httpc:request(get, {Url, []}, [], [{body_format, binary}]),
    case Res of
        {ok, {{_, Stat, _}, _, Data}} when 200=<Stat,Stat=<299->
            {value, jsone:decode(Data)};
        {ok, {{_, 404, _}, _, _}} ->
            none
    end;
lookup(Db0, Key0, #{url:=Url0}) ->
    Db1 = klsn_binstr:urlencode(Db0),
    Db = <<"/", Db1/binary>>,
    Key1 = klsn_binstr:urlencode(Key0),
    Key = <<"/", Key1/binary>>,
    Url = <<Url0/binary, Db/binary, Key/binary>>,
    Res = httpc:request(get, {Url, []}, [], [{body_format, binary}]),
    case Res of
        {ok, {{_, Stat, _}, _, Data}} when 200=<Stat,Stat=<299->
            {value, jsone:decode(Data)};
        {ok, {{_, 404, _}, _, _}} ->
            none
    end.


-spec bulk_lookup(db(), [key()]) -> [klsn:'maybe'(payload())].
bulk_lookup(Db, Keys) ->
    bulk_lookup(Db, Keys, db_info()).
-spec bulk_lookup(db(), [key()], info()) -> [klsn:'maybe'(payload())].
bulk_lookup(_Db, [], _Info) -> [];
bulk_lookup(Db, Keys0, #{url := Url0}) when is_list(Keys0) ->
    Keys = lists:map(fun klsn_binstr:from_any/1, Keys0),
    DbBin = klsn_binstr:urlencode(klsn_binstr:from_any(Db)),
    Path = <<"/", DbBin/binary, "/_all_docs?include_docs=true">>,
    Url = <<Url0/binary, Path/binary>>,
    Body = jsone:encode(#{<<"keys">> => Keys}),
    Res = httpc:request(post, {Url, [], "application/json", Body}, [], [{body_format, binary}]),
    case Res of
        {ok, {{_, Stat, _}, _, Data}} when 200 =< Stat, Stat =< 299 ->
            #{<<"rows">> := Rows} = jsone:decode(Data),
            lists:map(
                fun(Row) ->
                    case Row of
                        #{<<"error">> := _} -> none;
                        #{<<"doc">> := Doc} -> {value, Doc}
                    end
                end,
                Rows);
        _ ->
            lists:duplicate(length(Keys0), none)
    end.
-spec bulk_upsert(db(), [key()], upsert_function()) -> [payload()].
bulk_upsert(Db, Keys, Fun) ->
    bulk_upsert(Db, Keys, Fun, db_info()).
-spec bulk_upsert(db(), [key()], upsert_function(), info()) -> [payload()].
bulk_upsert(_Db, [], _Fun, _Info) -> [];
bulk_upsert(Db, Keys0, Fun, #{url := Url0} = Info) when is_list(Keys0) ->
    %% Fetch current documents once
    MaybeDocs = bulk_lookup(Db, Keys0, Info),

    %% Prepare updated/new documents using the callback
    TimeNow = time_now(),
    DocsPrepared = lists:map(
        fun({Key, MaybeDoc}) ->
            New0 = Fun(MaybeDoc),
            New1 = remove_keys(['_id', 'C', 'U'], New0),
            New2 = New1#{<<"_id">> => klsn_binstr:from_any(Key)},
            New3 = New2#{<<"U">> => TimeNow},
            case MaybeDoc of
                {value, #{<<"C">> := C}} -> New3#{<<"C">> => C};
                _ -> New3#{<<"C">> => TimeNow}
            end
        end,
        lists:zip(Keys0, MaybeDocs)
    ),

    %% Submit via _bulk_docs
    DbBin = klsn_binstr:urlencode(klsn_binstr:from_any(Db)),
    Path = <<"/", DbBin/binary, "/_bulk_docs">>,
    Url = <<Url0/binary, Path/binary>>,
    Body = jsone:encode(#{<<"docs">> => DocsPrepared}),
    Res = httpc:request(post, {Url, [], "application/json", Body}, [], [{body_format, binary}]),

    case Res of
        {ok, {{_, Stat, _}, _, Data}} when 200 =< Stat, Stat =< 299 ->
            Results = jsone:decode(Data),
            lists:map(
                fun({Doc0, ResRow}) ->
                    Doc = jsone:decode(jsone:encode(Doc0)),
                    case ResRow of
                        #{<<"ok">> := true, <<"id">> := Id, <<"rev">> := Rev} ->
                            Doc#{<<"_id">> => Id, <<"_rev">> => Rev};
                        #{<<"error">> := _} ->
                            %% Retry single-document upsert that already
                            %% has conflict–handling logic.
                            KeyBin = maps:get(<<"_id">>, Doc),
                            upsert(Db, KeyBin, Fun, Info)
                    end
                end,
                lists:zip(DocsPrepared, Results)
            )
    end.


-spec post(db(), payload(), info()) -> {id(), rev()}.
post(Db, Payload, Info) when is_atom(Db) ->
    post(atom_to_binary(Db), Payload, Info);
post(Db0, Payload0, #{url:=Url0}) ->
    Db1 = klsn_binstr:urlencode(Db0),
    Db = <<"/", Db1/binary>>,
    Payload = jsone:encode(Payload0),
    Url = <<Url0/binary, Db/binary>>,
    Res = httpc:request(post, {Url, [], "application/json", Payload}, [], [{body_format, binary}]),
    case Res of
        {ok, {{_, Stat, _}, _, Data}} when 200=<Stat,Stat=<299 ->
            #{<<"ok">>:=true,<<"id">>:=Id,<<"rev">>:=Rev} = jsone:decode(Data),
            {Id, Rev};
        {ok, {{_, 404, _}, _, _}} ->
            error(not_found);
        {ok, {{_, 409, _}, _, _}} ->
            error(conflict)
    end.

%% @doc
%% Read-modify-write helper.  Applies *Fun* to the current document and
%% stores the result. Fails with error:not_found when the key is absent.
-spec update(db(), key(), update_function()) -> payload().
update(Db, Key, Fun) ->
    update(Db, Key, Fun, db_info()).

%% @doc
%% Same as update/3 but with explicit Info.
-spec update(db(), key(), update_function(), info()) -> payload().
update(Db, Key, Fun0, Info) ->
    Fun = fun
        (none) ->
            error(not_found);
        ({value, Data}) ->
            Fun0(Data)
    end,
    upsert_(Db, Key, Fun, Info, 1).

%% @doc
%% Insert or update the document located at Key using Fun. Fun will
%% receive none on insert or {value, Old} on update and must return
%% the new payload.
-spec upsert(db(), key(), upsert_function()) -> payload().
upsert(Db, Key, Fun) ->
    upsert(Db, Key, Fun, db_info()).

%% @doc
%% Same as upsert/3 but with explicit Info.
-spec upsert(db(), key(), upsert_function(), info()
    ) -> payload().
upsert(_, <<>>, Fun, _) ->
    Fun(none);
upsert(Db, Key, Fun, Info) ->
    upsert_(Db, Key, Fun, Info, 1).

upsert_(_Db, _Key, _Fun, _Info, ReTry) when ReTry >= 10 ->
    error(too_many_retry);
upsert_(Db, {raw, Key}, Fun, Info, Retry) ->
    upsert_(Db, Key, Fun, Info, Retry);
upsert_(Db, Key, Fun, Info, Retry) ->
    MaybeData = lookup(Db, Key, Info),
    Data0 = Fun(MaybeData),
    Data1 = remove_keys(['_id', 'C', 'U'], Data0),
    Data2 = Data1#{<<"_id">>=>Key},
    TimeNow = time_now(),
    Data3 = Data2#{<<"U">>=>TimeNow},
    Data = case MaybeData of
        {value, #{<<"C">>:=C}} -> Data3#{<<"C">>=>C};
        _ -> Data3#{<<"C">>=>TimeNow}
    end,
    try
        post(Db, Data, Info)
    of
        {Id, Rev} ->
            Data#{
                <<"_id">> => Id
              , <<"_rev">> => Rev
            }
    catch
        error:conflict ->
            sleep(Retry),
            upsert_(Db, Key, Fun, Info, Retry+1);
        error:not_found ->
            error(not_found);
        throw:Error ->
            throw(Error);
        Class:Error:Stack ->
            spawn(fun()-> erlang:raise(Class,Error,Stack) end),
            sleep(Retry),
            upsert_(Db, Key, Fun, Info, Retry+5)
    end.


%% @doc
%% ISO-8601/RFC-3339 timestamp with millisecond precision and a fixed
%% +09:00 offset. Used in audit fields C (created) and U (updated).
-spec time_now() -> unicode:unicode_binary().
time_now() ->
    list_to_binary(calendar:system_time_to_rfc3339(erlang:system_time(millisecond), [{unit, millisecond}, {offset, "+09:00"}])).

-spec remove_keys([atom()], map()) -> map().
remove_keys(Keys, Map) when is_list(Keys), is_map(Map) ->
    lists:foldl(fun(Key, Data0) ->
        Data1 = maps:remove(Key, Data0),
        maps:remove(atom_to_binary(Key), Data1)
    end, Map, Keys).


%% @doc
%% Generate a monotonic-ish unique identifier suitable for use as a CouchDB
%% _id. Combines the current Unix time (seconds) with parts of an Erlang
%% reference encoded in base-36 so that the resulting IDs sort roughly in
%% creation order and stay URL-safe.

new_id() ->
    Ref = make_ref(),
    Time = erlang:system_time(second),
    Str0 = ref_to_list(Ref),
    [_|Str1] = lists:reverse(Str0),
    Str2 = lists:reverse(Str1),
    [_,A1,A2,A3] = string:split(Str2, ".", all),
    N1 = list_to_integer(A1),
    N2 = list_to_integer(A2),
    N3 = list_to_integer(A3),
    List = lists:flatten([
        string:casefold(integer_to_list(Time, 36)),
        "-",
        string:casefold(integer_to_list(N1, 36)),
        "-",
        string:casefold(integer_to_list(N2, 36)),
        "-",
        string:casefold(integer_to_list(N3, 36))
    ]),
    list_to_binary(List).

db_info() ->
    Url = case os:getenv("COUCHDB_URL") of
        false ->
            <<"http://localhost:5984">>;
        Str when is_list(Str) ->
            list_to_binary(Str)
    end,
    #{url=>Url}.

sleep(Stage) ->
    timer:sleep(round(1000 * rand:uniform() + 100 * math:exp(Stage))).
