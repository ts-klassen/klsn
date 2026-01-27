-module(klsn_bwrap_SUITE).
-include_lib("common_test/include/ct.hrl").

%% Export the suite callback and test cases
-export([
        all/0
      , echo_stdout/1
      , stdin_large_chunked/1
      , non_zero_exit_code/1
      , captures_stderr/1
      , enforces_timeout/1
      , bwrap_option_takes_effect/1
    ]).

all() ->
    [
        echo_stdout
      , stdin_large_chunked
      , non_zero_exit_code
      , captures_stderr
      , enforces_timeout
      , bwrap_option_takes_effect
    ].

echo_stdout(_Config) ->
    application:ensure_all_started(klsn),
    #{exit_code := 0, stdout := <<"hello\n">>, stderr := <<>>} =
        klsn_bwrap:run(
            [<<"/bin/echo">>, <<"hello">>]
          , #{
                bwrap => [
                    {ro_bind, <<"/">>, <<"/">>}
                  , {tmpfs, <<"/tmp">>}
                  , {proc, <<"/proc">>}
                  , {dev, <<"/dev">>}
                ]
              , timeout => 5000
            }
        ),
    ok.

stdin_large_chunked(_Config) ->
    application:ensure_all_started(klsn),
    Payload = binary:copy(<<"abcdefghijklmnopqrstuvwxyz0123456789\n">>, 4000),
    #{exit_code := 0, stdout := Payload, stderr := <<>>} =
        klsn_bwrap:run(
            [<<"/bin/cat">>]
          , #{
                bwrap => [
                    {ro_bind, <<"/">>, <<"/">>}
                  , {tmpfs, <<"/tmp">>}
                  , {proc, <<"/proc">>}
                  , {dev, <<"/dev">>}
                ]
              , stdin => Payload
              , timeout => 5000
            }
        ),
    ok.

non_zero_exit_code(_Config) ->
    application:ensure_all_started(klsn),
    #{exit_code := 7, stdout := <<>>, stderr := <<>>} =
        klsn_bwrap:run(
            [<<"/bin/sh">>, <<"-lc">>, <<"exit 7">>]
          , #{
                bwrap => [
                    {ro_bind, <<"/">>, <<"/">>}
                  , {tmpfs, <<"/tmp">>}
                  , {proc, <<"/proc">>}
                  , {dev, <<"/dev">>}
                ]
              , timeout => 5000
            }
        ),
    ok.

captures_stderr(_Config) ->
    application:ensure_all_started(klsn),
    #{exit_code := 0, stdout := <<"out\n">>, stderr := <<"err\n">>} =
        klsn_bwrap:run(
            [<<"/bin/sh">>, <<"-lc">>, <<"echo out; echo err 1>&2">>]
          , #{
                bwrap => [
                    {ro_bind, <<"/">>, <<"/">>}
                  , {tmpfs, <<"/tmp">>}
                  , {proc, <<"/proc">>}
                  , {dev, <<"/dev">>}
                ]
              , timeout => 5000
            }
        ),
    ok.

enforces_timeout(_Config) ->
    application:ensure_all_started(klsn),
    %% Intentionally short timeout to ensure the call fails deterministically.
    ok = try
        klsn_bwrap:run(
            [<<"/bin/sh">>, <<"-lc">>, <<"sleep 2">>]
          , #{
                bwrap => [
                    {ro_bind, <<"/">>, <<"/">>}
                  , {tmpfs, <<"/tmp">>}
                  , {proc, <<"/proc">>}
                  , {dev, <<"/dev">>}
                ]
              , timeout => 50
            }
        ),
        error(unexpected_success)
    catch
        error:timeout ->
            ok
    end,
    ok.

bwrap_option_takes_effect(_Config) ->
    application:ensure_all_started(klsn),
    %% chdir+dir should affect the process working directory.
    UniqueDir = iolist_to_binary([
        <<"/tmp/ct-">>,
        klsn_binstr:uuid()
    ]),
    Size = byte_size(UniqueDir),
    #{exit_code := 0, stdout := <<UniqueDir:Size/binary, "\n">>, stderr := <<>>} =
        klsn_bwrap:run(
            [<<"/bin/pwd">>]
          , #{
                bwrap => [
                    {ro_bind, <<"/">>, <<"/">>}
                  , {tmpfs, <<"/tmp">>}
                  , {proc, <<"/proc">>}
                  , {dev, <<"/dev">>}
                  , {dir, UniqueDir}
                  , {chdir, UniqueDir}
                ]
              , timeout => 5000
            }
        ),
    ok.
