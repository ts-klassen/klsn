-module(klsn_bwrap_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("kernel/include/file.hrl").

%% Export the suite callback and test cases
-export([
        all/0
      , echo_stdout/1
      , stdin_large_chunked/1
      , non_zero_exit_code/1
      , captures_stderr/1
      , enforces_timeout/1
      , bwrap_option_takes_effect/1
      , stdin_eof/1
      , reopens_stdin_after_eof/1
      , streaming_reopens_stdin/1
      , streaming_roundtrip/1
      , streaming_binary_io/1
      , streaming_exit_status/1
      , send_after_completion/1
      , stale_stream_handles/1
      , streaming_backpressure/1
      , stdin_closes_while_draining/1
      , streaming_stop/1
      , concurrent_streams/1
      , literal_arguments/1
      , timeout_with_blocked_stdin/1
      , invalid_arguments/1
      , cleans_up/1
      , stop_new_session/1
      , stop_term_children/1
      , killed_exec_process/1
      , linked_owner_exit/1
      , unrelated_link_exit/1
      , eof_is_idempotent/1
      , temporary_path_arguments/1
      , failed_start_cleans_up/1
      , closes_output_before_exit/1
      , exits_without_reading_stdin/1
      , external_signal_status/1
      , background_holds_output/1
      , continuous_background_output/1
      , private_temporary_directory/1
      , background_survives_completion/1
      , background_reopens_output/1
      , large_output/1
      , packet_output/1
      , large_input/1
      , restrictive_inherited_umask/1
      , invalid_inherited_locale/1
      , inherited_ignored_hup/1
      , inherited_ignored_int/1
      , inherited_blocked_signals/1
      , inherited_read_timeout/1
      , inherited_control_environment/1
      , large_inherited_environment/1
      , inherited_native_environment/1
      , inherited_posix_mode/1
      , inherited_shell_options/1
      , inherited_shell_functions/1
      , equals_executable_paths/1
      , relative_executable_paths/1
      , control_pipe_disconnect/1
      , vm_shutdown_cleans_up/1
      , reader_shutdown_cleans_up/1
      , cwd_changes_after_open/1
      , startup_cleanup/1
      , helper_ownership/1
      , startup_timeout/1
      , timeout_during_diagnostic_read/1
      , timeout_while_draining/1
      , stop_while_draining/1
      , stop_during_diagnostic_read/1
      , background_survives_before_wait/1
      , stop_with_blocked_file_server/1
      , control_exit_with_blocked_file_server/1
      , completion_with_blocked_file_server/1
    ]).

all() ->
    [
        echo_stdout
      , stdin_large_chunked
      , non_zero_exit_code
      , captures_stderr
      , enforces_timeout
      , bwrap_option_takes_effect
      , stdin_eof
      , reopens_stdin_after_eof
      , streaming_reopens_stdin
      , streaming_roundtrip
      , streaming_binary_io
      , streaming_exit_status
      , send_after_completion
      , stale_stream_handles
      , streaming_backpressure
      , stdin_closes_while_draining
      , streaming_stop
      , concurrent_streams
      , literal_arguments
      , timeout_with_blocked_stdin
      , invalid_arguments
      , cleans_up
      , stop_new_session
      , stop_term_children
      , killed_exec_process
      , linked_owner_exit
      , unrelated_link_exit
      , eof_is_idempotent
      , temporary_path_arguments
      , failed_start_cleans_up
      , closes_output_before_exit
      , exits_without_reading_stdin
      , external_signal_status
      , background_holds_output
      , continuous_background_output
      , private_temporary_directory
      , background_survives_completion
      , background_reopens_output
      , large_output
      , packet_output
      , large_input
      , restrictive_inherited_umask
      , invalid_inherited_locale
      , inherited_ignored_hup
      , inherited_ignored_int
      , inherited_blocked_signals
      , inherited_read_timeout
      , inherited_control_environment
      , large_inherited_environment
      , inherited_native_environment
      , inherited_posix_mode
      , inherited_shell_options
      , inherited_shell_functions
      , equals_executable_paths
      , relative_executable_paths
      , control_pipe_disconnect
      , vm_shutdown_cleans_up
      , reader_shutdown_cleans_up
      , cwd_changes_after_open
      , startup_cleanup
      , helper_ownership
      , startup_timeout
      , timeout_during_diagnostic_read
      , timeout_while_draining
      , stop_while_draining
      , stop_during_diagnostic_read
      , background_survives_before_wait
      , stop_with_blocked_file_server
      , control_exit_with_blocked_file_server
      , completion_with_blocked_file_server
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

stdin_eof(_Config) ->
    Expected = #{exit_code => 0, stdout => <<>>, stderr => <<>>},
    ?assertEqual(Expected, klsn_bwrap:run([<<"/bin/cat">>], run_opts())),
    ?assertEqual(Expected, klsn_bwrap:run([<<"/bin/cat">>],
        (run_opts())#{stdin => <<>>})).

reopens_stdin_after_eof(_Config) ->
    lists:foreach(fun(Path) ->
        lists:foreach(fun({InputOpts, Data}) ->
            Opts = maps:merge(run_opts(), InputOpts),
            ?assertEqual(#{exit_code => 0, stdout => Data, stderr => <<>>},
                klsn_bwrap:run([<<"/bin/cat">>, Path], Opts)),
            ?assertEqual(#{exit_code => 0, stdout => <<Data/binary, "done">>, stderr => <<>>},
                klsn_bwrap:run([<<"/bin/sh">>, <<"-c">>,
                    <<"cat; cat \"$1\"; printf done">>, <<"sh">>, Path], Opts))
        end, [{#{}, <<>>}, {#{stdin => <<>>}, <<>>},
            {#{stdin => <<"hello\n">>}, <<"hello\n">>}])
    end, [<<"/dev/stdin">>, <<"/proc/self/fd/0">>]).

streaming_reopens_stdin(_Config) ->
    Handle = klsn_bwrap:open([<<"/bin/sh">>, <<"-c">>,
        <<"cat; cat /dev/stdin; cat /proc/self/fd/0; printf done">>],
        (open_opts())#{stdin => <<"initial\n">>}),
    #{os_pid := OsPid} = Handle,
    try
        expect_stdout(OsPid, <<"initial\n">>),
        ok = klsn_bwrap:send(Handle, <<"second\n">>),
        expect_stdout(OsPid, <<"second\n">>),
        ok = klsn_bwrap:send_eof(Handle),
        ?assertEqual({normal, <<"done">>, <<>>}, collect(Handle))
    after
        catch klsn_bwrap:stop(Handle)
    end.

streaming_roundtrip(_Config) ->
    Handle = klsn_bwrap:open(
        [<<"/bin/sh">>, <<"-c">>, <<"cat; printf done >&2">>],
        (open_opts())#{stdin => <<"initial\n">>}),
    #{os_pid := OsPid, exec_pid := ExecPid} = Handle,
    ?assert(is_integer(OsPid)),
    ?assert(is_pid(ExecPid)),
    %% Output must arrive while stdin is still open.
    expect_stdout(OsPid, <<"initial\n">>),
    %% The public functions have always accepted an os_pid-only handle.
    ok = klsn_bwrap:send(#{os_pid => OsPid}, <<"second\n">>),
    expect_stdout(OsPid, <<"second\n">>),
    ok = klsn_bwrap:send_eof(#{os_pid => OsPid}),
    ?assertEqual({normal, <<>>, <<"done">>}, collect(Handle)).

streaming_binary_io(_Config) ->
    %% Exercise every byte value, multiple port buffers, both output streams,
    %% and EOF queued immediately after a large write.
    Payload = binary:copy(list_to_binary(lists:seq(0, 255)), 8192),
    Handle = klsn_bwrap:open([<<"/usr/bin/tee">>, <<"/dev/stderr">>], open_opts()),
    ok = klsn_bwrap:send(Handle, Payload),
    ok = klsn_bwrap:send_eof(Handle),
    ?assertEqual({normal, Payload, Payload}, collect(Handle)).

streaming_exit_status(_Config) ->
    lists:foreach(fun(Code) ->
        Command = <<"printf out; printf err >&2; exit ", (integer_to_binary(Code))/binary>>,
        Handle = klsn_bwrap:open([<<"/bin/sh">>, <<"-c">>, Command], open_opts()),
        Reason = case Code of
            0 -> normal;
            _ -> {exit_status, Code bsl 8}
        end,
        ?assertEqual({Reason, <<"out">>, <<"err">>}, collect(Handle))
    end, [0, 1, 7, 127, 128, 137, 143, 255]),
    ?assertMatch(#{exit_code := 143}, klsn_bwrap:run(
        [<<"/bin/sh">>, <<"-c">>, <<"kill -TERM $$">>], run_opts())).

send_after_completion(_Config) ->
    Handle = klsn_bwrap:open([<<"/usr/bin/head">>, <<"-c">>, <<"1">>], open_opts()),
    #{os_pid := OsPid, exec_pid := ExecPid} = Handle,
    Monitor = erlang:monitor(process, ExecPid),
    ok = klsn_bwrap:send(Handle, <<"x">>),
    %% Await the worker independently, leaving the public DOWN queued as it
    %% would be when a caller sends another chunk before reading completion.
    receive
        {'DOWN', Monitor, process, ExecPid, Reason} -> ?assertEqual(normal, Reason)
    after 5000 ->
        ct:fail(exec_process_did_not_exit)
    end,
    lists:foreach(fun(Target) ->
        ?assertEqual(ok, klsn_bwrap:send(Target, <<"late">>)),
        ?assertEqual(ok, klsn_bwrap:send(Target, <<>>)),
        ?assertEqual(ok, klsn_bwrap:send_eof(Target))
    end, [Handle, #{os_pid => OsPid}]),
    ?assertEqual({normal, <<"x">>, <<>>}, collect(Handle)).

stale_stream_handles(_Config) ->
    Old = #{exec_pid := Dead} = klsn_bwrap:open([<<"/bin/true">>], open_opts()),
    ?assertEqual({normal, <<>>, <<>>}, collect(Old)),
    wait_until(fun() -> not is_process_alive(Dead) end, 100),
    lists:foreach(fun({Identity, Action}) ->
        Live = #{os_pid := OsPid} = klsn_bwrap:open([<<"/bin/cat">>], open_opts()),
        %% Model OS PID reuse deterministically without changing native PIDs.
        Stale = Old#{os_pid := OsPid, exec_pid := Identity},
        try
            case Action of
                send -> ok = klsn_bwrap:send(Stale, <<"stale-input">>);
                eof -> ok = klsn_bwrap:send_eof(Stale);
                stop -> ?assertError({badmatch, {error, no_process}},
                    klsn_bwrap:stop(Stale))
            end,
            ok = klsn_bwrap:send(Live, <<"full-handle\n">>),
            ok = klsn_bwrap:send(#{os_pid => OsPid}, <<"pid-only\n">>),
            ok = klsn_bwrap:send_eof(Live),
            ?assertEqual({normal, <<"full-handle\npid-only\n">>, <<>>}, collect(Live))
        after
            catch klsn_bwrap:stop(Live)
        end
    end, [{Identity, Action} || Identity <- [Dead, invalid], Action <- [send, eof, stop]]).

streaming_backpressure(Config) ->
    lists:foreach(fun(Mode) -> streaming_backpressure(Config, Mode) end, [read, stop]).

streaming_backpressure(Config, Mode) ->
    Dir = filename:join(?config(priv_dir, Config), "backpressure-" ++ atom_to_list(Mode)),
    Gate = filename:join(Dir, "read"),
    ok = file:make_dir(Dir),
    Opts = open_opts(),
    Handle = #{os_pid := OsPid} = klsn_bwrap:open([<<"/bin/sh">>, <<"-c">>,
        <<"printf ready; while [ ! -f /tmp/gate/read ]; do sleep .01; done; exec cat">>],
        Opts#{bwrap := maps:get(bwrap, Opts) ++
            [{bind, unicode:characters_to_binary(Dir), <<"/tmp/gate">>}]}),
    Parent = self(),
    Chunks = [binary:copy(<<N>>, 1024 * 1024) || N <- lists:seq(1, 8)],
    {Sender, Monitor} = spawn_monitor(fun() ->
        receive send -> ok end,
        lists:foreach(fun(Data) ->
            ok = klsn_bwrap:send(Handle, Data),
            Parent ! {self(), accepted}
        end, Chunks),
        ok = klsn_bwrap:send_eof(Handle)
    end),
    try
        expect_stdout(OsPid, <<"ready">>),
        Sender ! send,
        receive {Sender, accepted} -> ok
            after 5000 -> ct:fail(first_send_not_accepted) end,
        %% The native queue admits a small prefix; a non-reader must keep
        %% the sequential producer from acknowledging all eight MiB.
        timer:sleep(200),
        Accepted = accepted_input(Sender, 1),
        ?assert(Accepted =< 3),
        ?assert(is_process_alive(Sender)),
        Start = erlang:monotonic_time(millisecond),
        case Mode of
            read -> ok = file:write_file(Gate, <<>>);
            stop -> ok = klsn_bwrap:stop(Handle)
        end,
        wait_input_sender({Sender, Monitor}),
        case Mode of
            read -> ?assertEqual({normal, iolist_to_binary(Chunks), <<>>}, collect(Handle));
            stop ->
                ?assertMatch({{exit_status, 15}, _, _}, collect(Handle)),
                ?assert(erlang:monotonic_time(millisecond) - Start < 2500)
        end,
        accepted_input(Sender, 0)
    after
        file:write_file(Gate, <<>>),
        catch klsn_bwrap:stop(Handle),
        exit(Sender, kill),
        erlang:demonitor(Monitor, [flush]),
        file:delete(Gate),
        file:del_dir(Dir)
    end.

accepted_input(Sender, Count) ->
    receive {Sender, accepted} -> accepted_input(Sender, Count + 1)
    after 0 -> Count end.

wait_input_sender(none) -> ok;
wait_input_sender({Sender, Monitor}) ->
    receive {'DOWN', Monitor, process, Sender, Reason} -> ?assertEqual(normal, Reason)
    after 5000 -> ct:fail(input_sender_still_blocked) end.

stdin_closes_while_draining(Config) ->
    Tools = filename:join(?config(priv_dir, Config), "draining-tools"),
    Dd = filename:join(Tools, "dd"),
    RealDd = os:find_executable("dd"),
    OriginalPath = os:getenv("PATH"),
    ok = file:make_dir(Tools),
    %% Hold final reads after WAIT, so worker completion cannot close stdin
    %% on behalf of the exit-status transition being exercised here.
    ok = file:write_file(Dd,
        <<"#!/bin/sh\n"
          "for arg; do\n"
          "  if [ \"$arg\" = count=4194304 ]; then\n"
          "    : >\"$KLSN_CT_DRAIN_STARTED\"\n"
          "    i=0\n"
          "    while [ ! -f \"$KLSN_CT_DRAIN_RELEASE\" ]; do\n"
          "      i=$((i+1)); [ \"$i\" -lt 1000 ] || exit 99\n"
          "      sleep 0.01\n"
          "    done\n"
          "    break\n"
          "  fi\n"
          "done\n"
          "exec \"$KLSN_CT_REAL_DD\" \"$@\"\n">>),
    ok = file:change_mode(Dd, 8#755),
    try
        lists:foreach(fun(Mode) ->
            Dir = filename:join(?config(priv_dir, Config), atom_to_list(Mode)),
            ok = file:make_dir(Dir),
            try
                with_environment([{"PATH", Tools ++ ":" ++ OriginalPath},
                        {"KLSN_CT_REAL_DD", RealDd},
                        {"KLSN_CT_DRAIN_STARTED", filename:join(Dir, "draining")},
                        {"KLSN_CT_DRAIN_RELEASE", filename:join(Dir, "release")}], fun() ->
                    stdin_during_drain(Dir, Mode)
                end)
            after
                file:del_dir(Dir)
            end
        end, [empty_input, queued_input, flushing_eof])
    after
        file:delete(Dd),
        file:del_dir(Tools)
    end.

stdin_during_drain(Dir, Mode) ->
    [Exit, Read, Capture, Eof, Draining, Release] = Paths =
        [filename:join(Dir, Name) || Name <- ["exit", "read", "capture", "eof", "draining", "release"]],
    Opts = open_opts(),
    BindDir = unicode:characters_to_binary(Dir),
    Args = [unicode:characters_to_binary(Path) || Path <- [Exit, Read, Capture, Eof]],
    Handle = klsn_bwrap:open([<<"/bin/sh">>, <<"-c">>,
        %% Background jobs default to /dev/null stdin unless redirected.
        <<"exec 7<&0; (i=0; while [ ! -f \"$2\" ]; do "
          "i=$((i+1)); [ \"$i\" -lt 1000 ] || exit 1; sleep 0.01; done; "
          "cat <&7 >\"$3\"; printf eof >\"$4\") & exec 7<&-; "
          "printf out; printf err >&2; i=0; while [ ! -f \"$1\" ]; do "
          "i=$((i+1)); [ \"$i\" -lt 1000 ] || exit 1; sleep 0.01; done">>,
        <<"sh">> | Args], Opts#{bwrap := maps:get(bwrap, Opts) ++ [{bind, BindDir, BindDir}]}),
    #{exec_pid := ExecPid, os_pid := OsPid} = Handle,
    Monitor = erlang:monitor(process, ExecPid),
    try
        {Size, Sender} = queue_before_exit(Handle, Mode),
        ok = file:write_file(Exit, <<>>),
        wait_until(fun() -> filelib:is_file(Draining) end, 500),
        wait_input_sender(Sender),
        ?assert(is_process_alive(ExecPid)),
        %% Both address forms must discard sends while the worker drains.
        ok = klsn_bwrap:send(Handle, <<"late-full-handle">>),
        ok = klsn_bwrap:send(#{os_pid => OsPid}, <<"late-os-pid">>),
        ok = file:write_file(Read, <<>>),
        wait_until(fun() -> file:read_file(Eof) =:= {ok, <<"eof">>} end, 200),
        ?assert(is_process_alive(ExecPid)),
        {ok, Captured} = file:read_file(Capture),
        %% Bytes already in the anonymous pipe may arrive, but queued input
        %% and every send after WAIT must be discarded.
        ?assertEqual(binary:copy(<<0>>, byte_size(Captured)), Captured),
        case Size of
            0 -> ?assertEqual(<<>>, Captured);
            _ -> ?assert(byte_size(Captured) < Size)
        end,
        ok = file:write_file(Release, <<>>),
        ?assertEqual({normal, <<"out">>, <<"err">>}, collect(Handle))
    after
        %% Release readers and the surviving child even if an assertion
        %% fails; EOF lets the old implementation's descendant finish too.
        file:write_file(Exit, <<>>),
        file:write_file(Read, <<>>),
        file:write_file(Release, <<>>),
        catch klsn_bwrap:send_eof(Handle),
        receive
            {'DOWN', Monitor, process, ExecPid, _} -> ok
        after 5000 ->
            exit(ExecPid, kill),
            erlang:demonitor(Monitor, [flush])
        end,
        [file:delete(Path) || Path <- Paths]
    end.

queue_before_exit(_Handle, empty_input) -> {0, none};
queue_before_exit(#{exec_pid := ExecPid} = Handle, queued_input) ->
    Size = 16 * 1024 * 1024,
    %% Backpressure holds this producer until the command exits. Keep the
    %% owner free to release the exit gate and check the final output drain.
    1 = erlang:trace(ExecPid, true, ['receive']),
    try
        {Sender, _} = Pending = spawn_monitor(fun() ->
            ok = klsn_bwrap:send(Handle, binary:copy(<<0>>, Size))
        end),
        receive
            {trace, ExecPid, 'receive', {request, Sender, _, {send, _}}} -> ok
        after 5000 -> ct:fail(input_not_queued)
        end,
        {Size, Pending}
    after
        catch erlang:trace(ExecPid, false, ['receive'])
    end;
queue_before_exit(#{exec_pid := ExecPid} = Handle, flushing_eof) ->
    Size = 1024 * 1024,
    %% Observe the queued close so this case exercises EOF already in
    %% flight, while the descendant still refuses to read from the pipe.
    1 = erlang:trace(ExecPid, true, [send]),
    try
        ok = klsn_bwrap:send(Handle, binary:copy(<<0>>, Size)),
        ok = klsn_bwrap:send_eof(Handle),
        receive
            {trace, ExecPid, send, {close_input, _Port}, _Guardian} -> ok
        after 5000 -> ct:fail(input_close_not_queued)
        end
    after
        catch erlang:trace(ExecPid, false, [send])
    end,
    {Size, none}.

streaming_stop(_Config) ->
    Handle = klsn_bwrap:open(
        [<<"/bin/sh">>, <<"-c">>, <<"printf ready; exec sleep 30">>], open_opts()),
    #{os_pid := OsPid, exec_pid := ExecPid} = Handle,
    expect_stdout(OsPid, <<"ready">>),
    Monitor = erlang:monitor(process, ExecPid),
    ok = klsn_bwrap:stop(#{os_pid => OsPid}),
    ?assertMatch({{exit_status, 15}, _, _}, collect(Handle)),
    receive
        {'DOWN', Monitor, process, ExecPid, {exit_status, 15}} -> ok
    after 5000 ->
        ct:fail(exec_process_did_not_exit)
    end.

concurrent_streams(_Config) ->
    Streams = [begin
        Data = integer_to_binary(N),
        Handle = klsn_bwrap:open([<<"/bin/cat">>], open_opts()),
        ok = klsn_bwrap:send(Handle, Data),
        {Handle, Data}
    end || N <- lists:seq(1, 12)],
    lists:foreach(fun({Handle, _}) -> ok = klsn_bwrap:send_eof(Handle) end, Streams),
    lists:foreach(fun({Handle, Data}) ->
        ?assertEqual({normal, Data, <<>>}, collect(Handle))
    end, Streams).

literal_arguments(_Config) ->
    Args = [<<>>, <<"two words">>, <<"'\"\\; $HOME `true` $(true) *\n">>,
        unicode:characters_to_binary([26085, 26412, 35486])],
    Expected = iolist_to_binary([[Arg, 0] || Arg <- Args]),
    ?assertEqual(#{exit_code => 0, stdout => Expected, stderr => <<>>},
        klsn_bwrap:run([<<"/usr/bin/printf">>, <<"%s\\0">> | Args], run_opts())).

timeout_with_blocked_stdin(Config) ->
    Dir = filename:join(?config(priv_dir, Config), "blocked-stdin"),
    Ready = filename:join(Dir, "ready"),
    ok = file:make_dir(Dir),
    Opts = run_opts(),
    try
        Start = erlang:monotonic_time(millisecond),
        ?assertError(timeout, klsn_bwrap:run([<<"/bin/sh">>, <<"-c">>,
            <<"printf ready >/tmp/checkpoint/ready; exec sleep 30">>],
            Opts#{bwrap := maps:get(bwrap, Opts) ++
                [{bind, unicode:characters_to_binary(Dir), <<"/tmp/checkpoint">>}],
                stdin => binary:copy(<<0>>, 16 * 1024 * 1024), timeout => 1000})),
        ?assert(erlang:monotonic_time(millisecond) - Start < 3000),
        %% A startup timeout must not satisfy this runtime backpressure test.
        ?assertEqual({ok, <<"ready">>}, file:read_file(Ready))
    after
        file:delete(Ready),
        file:del_dir(Dir)
    end.

invalid_arguments(_Config) ->
    ?assertError(badarg, klsn_bwrap:run([<<"/bin/true">>],
        (run_opts())#{stdin => "not binary"})),
    ?assertError(badarg, klsn_bwrap:open([<<"/bin/true">>],
        (open_opts())#{stdin => "not binary"})),
    ?assertError(badarg, klsn_bwrap:run([<<"/bin/true">>],
        (run_opts())#{timeout => -1})),
    ?assertError(badarg, klsn_bwrap:run([], #{bwrap => [unknown_option]})),
    ?assertError(badarg, klsn_bwrap:send(#{}, <<>>)),
    ?assertError(badarg, klsn_bwrap:send_eof(#{})),
    ?assertError(badarg, klsn_bwrap:stop(#{})).

cleans_up(Config) ->
    TrapExit = process_info(self(), trap_exit),
    %% Earlier timeouts return while their guardians still remove resources.
    %% Only this case's private root belongs in its cleanup assertion.
    Tmp = filename:join(?config(priv_dir, Config), "cleanup-tmp"),
    ok = file:make_dir(Tmp),
    try
        with_environment([{"TMPDIR", Tmp}], fun() ->
            lists:foreach(fun(_) ->
                ?assertMatch(#{exit_code := 0},
                    klsn_bwrap:run([<<"/bin/true">>], run_opts()))
            end, lists:seq(1, 10)),
            ?assertEqual({ok, []}, file:list_dir(Tmp))
        end)
    after
        file:del_dir(Tmp)
    end,
    ?assertEqual(TrapExit, process_info(self(), trap_exit)),
    receive
        {Port, _} when is_port(Port) -> ct:fail({leaked_port_message, Port});
        {'EXIT', Port, _} when is_port(Port) -> ct:fail({leaked_port_exit, Port})
    after 0 -> ok
    end.

stop_new_session(_Config) ->
    lists:foreach(fun(Extra) ->
        Opts = open_opts(),
        Handle = klsn_bwrap:open(
            [<<"/bin/sh">>, <<"-c">>, <<"printf ready; exec sleep 30">>],
            Opts#{bwrap := maps:get(bwrap, Opts) ++ Extra}),
        #{os_pid := OsPid} = Handle,
        expect_stdout(OsPid, <<"ready">>),
        ok = klsn_bwrap:stop(Handle),
        ?assertMatch({{exit_status, _}, _, _}, collect(Handle))
    end, [[new_session], [unshare_pid, new_session]]).

stop_term_children(Config) ->
    vm_fixture(Config, "stop_term_children", [], [],
        <<"TERM-created children removed; raw statuses preserved\n">>).

killed_exec_process(_Config) ->
    Tmp = case os:getenv("TMPDIR") of false -> "/tmp"; Path -> Path end,
    Pattern = filename:join(Tmp, "klsn-bwrap-" ++ os:getpid() ++ "-*"),
    Before = filelib:wildcard(Pattern),
    Opts = open_opts(),
    #{os_pid := OsPid, exec_pid := ExecPid} = klsn_bwrap:open(
        [<<"/bin/sh">>, <<"-c">>, <<"printf ready; exec sleep 30">>],
        Opts#{bwrap := [new_session | maps:get(bwrap, Opts)]}),
    expect_stdout(OsPid, <<"ready">>),
    Monitor = erlang:monitor(process, ExecPid),
    exit(ExecPid, kill),
    receive
        {'DOWN', Monitor, process, ExecPid, killed} -> ok
    after 5000 -> ct:fail(exec_process_still_alive)
    end,
    wait_until(fun() ->
        filelib:wildcard(Pattern) =:= Before andalso
            not filelib:is_file("/proc/" ++ integer_to_list(OsPid) ++ "/stat")
    end, 200).

linked_owner_exit(_Config) ->
    Tmp = case os:getenv("TMPDIR") of false -> "/tmp"; Path -> Path end,
    Pattern = filename:join(Tmp, "klsn-bwrap-" ++ os:getpid() ++ "-*"),
    Before = filelib:wildcard(Pattern),
    lists:foreach(fun(ExitReason) ->
        Parent = self(),
        Owner = spawn(fun() ->
            Handle = klsn_bwrap:open([<<"/bin/cat">>], open_opts()),
            link(maps:get(exec_pid, Handle)),
            Parent ! {self(), Handle},
            receive finish -> exit(ExitReason) end
        end),
        receive
            {Owner, #{exec_pid := ExecPid, os_pid := OsPid} = Handle} ->
                Monitor = erlang:monitor(process, ExecPid),
                try
                    Expected = case ExitReason of
                        normal -> normal;
                        shutdown -> shutdown;
                        kill -> {owner_died, Owner, killed};
                        _ -> {owner_died, Owner, ExitReason}
                    end,
                    case ExitReason of
                        kill -> exit(Owner, kill);
                        _ -> Owner ! finish
                    end,
                    receive
                        {'DOWN', Monitor, process, ExecPid, Reason} ->
                            ?assertEqual(Expected, Reason)
                    after 2000 -> ct:fail(linked_worker_still_alive)
                    end,
                    wait_until(fun() ->
                        filelib:wildcard(Pattern) =:= Before andalso
                            not filelib:is_file("/proc/" ++ integer_to_list(OsPid) ++ "/stat")
                    end, 200)
                after
                    erlang:demonitor(Monitor, [flush]),
                    exit(Owner, kill),
                    catch klsn_bwrap:stop(Handle)
                end
        after 5000 ->
            exit(Owner, kill),
            ct:fail(owner_did_not_start)
        end
    end, [normal, shutdown, custom_reason, kill]).

unrelated_link_exit(_Config) ->
    Handle = klsn_bwrap:open([<<"/bin/cat">>], open_opts()),
    #{exec_pid := ExecPid, os_pid := OsPid} = Handle,
    try
        lists:foreach(fun(Reason) ->
            {Other, Monitor} = spawn_monitor(fun() -> link(ExecPid), exit(Reason) end),
            receive {'DOWN', Monitor, process, Other, Reason} -> ok
            after 5000 -> ct:fail(linked_process_still_alive)
            end,
            ok = klsn_bwrap:send(Handle, <<"alive">>),
            expect_stdout(OsPid, <<"alive">>)
        end, [normal, shutdown, unrelated_failure]),
        ok = klsn_bwrap:send_eof(Handle),
        ?assertEqual({normal, <<>>, <<>>}, collect(Handle))
    after
        catch klsn_bwrap:stop(Handle)
    end.

eof_is_idempotent(Config) ->
    Dir = unicode:characters_to_binary(?config(priv_dir, Config)),
    Marker = filename:join(Dir, <<"eof-finished">>),
    Opts = open_opts(),
    Handle = klsn_bwrap:open([<<"/bin/sh">>, <<"-c">>,
        <<"cat; printf eof; while [ ! -f \"$1\" ]; do sleep 0.01; done; printf done">>,
        <<"sh">>, Marker], Opts#{bwrap := maps:get(bwrap, Opts) ++ [{bind, Dir, Dir}]}),
    #{os_pid := OsPid} = Handle,
    try
        ok = klsn_bwrap:send_eof(Handle),
        expect_stdout(OsPid, <<"eof">>),
        ok = klsn_bwrap:send_eof(Handle),
        ok = klsn_bwrap:send(Handle, <<"ignored">>),
        ok = file:write_file(Marker, <<>>),
        ?assertEqual({normal, <<"done">>, <<>>}, collect(Handle))
    after
        file:delete(Marker)
    end.

temporary_path_arguments(Config) ->
    OldTmp = os:getenv("TMPDIR"),
    Tmp = filename:join(?config(priv_dir, Config), "space ' \" $() temporary"),
    ok = file:make_dir(Tmp),
    try
        true = os:putenv("TMPDIR", Tmp),
        ?assertEqual(#{exit_code => 0, stdout => <<"data">>, stderr => <<>>},
            klsn_bwrap:run([<<"/bin/cat">>], (run_opts())#{stdin => <<"data">>})),
        ?assertEqual({ok, []}, file:list_dir(Tmp))
    after
        case OldTmp of
            false -> os:unsetenv("TMPDIR");
            _ -> os:putenv("TMPDIR", OldTmp)
        end,
        file:del_dir(Tmp)
    end.

failed_start_cleans_up(Config) ->
    OldTmp = os:getenv("TMPDIR"),
    Tmp = filename:join(?config(priv_dir, Config), "failed-start"),
    ok = file:make_dir(Tmp),
    try
        true = os:putenv("TMPDIR", Tmp),
        ?assertError(badarg, klsn_bwrap:open([#{invalid => true}], open_opts())),
        ?assertEqual({ok, []}, file:list_dir(Tmp)),
        ?assertMatch(#{exit_code := 1}, klsn_bwrap:run(
            [<<"/this-command-does-not-exist">>], run_opts())),
        ?assertEqual({ok, []}, file:list_dir(Tmp))
    after
        case OldTmp of
            false -> os:unsetenv("TMPDIR");
            _ -> os:putenv("TMPDIR", OldTmp)
        end,
        file:del_dir(Tmp)
    end.

closes_output_before_exit(_Config) ->
    ?assertEqual(#{exit_code => 7, stdout => <<>>, stderr => <<>>},
        klsn_bwrap:run([<<"/bin/sh">>, <<"-c">>,
            <<"exec 1>&- 2>&-; sleep 0.05; exit 7">>], run_opts())).

exits_without_reading_stdin(_Config) ->
    ?assertEqual(#{exit_code => 0, stdout => <<>>, stderr => <<>>},
        klsn_bwrap:run([<<"/bin/true">>],
            (run_opts())#{stdin => binary:copy(<<0>>, 16 * 1024 * 1024)})).

external_signal_status(_Config) ->
    lists:foreach(fun({Signal, Raw}) ->
        Handle = klsn_bwrap:open([<<"/bin/sh">>, <<"-c">>,
            <<"printf ready; exec sleep 30">>], open_opts()),
        #{os_pid := OsPid, exec_pid := ExecPid} = Handle,
        expect_stdout(OsPid, <<"ready">>),
        Monitor = erlang:monitor(process, ExecPid),
        Port = open_port({spawn_executable, os:find_executable("kill")},
            [exit_status, {args, ["-" ++ Signal, "--", integer_to_list(OsPid)]}]),
        receive {Port, {exit_status, 0}} -> ok after 5000 -> ct:fail(kill_failed) end,
        ?assertEqual({{exit_status, Raw}, <<>>, <<>>}, collect(Handle)),
        receive
            {'DOWN', Monitor, process, ExecPid, {exit_status, Raw}} -> ok
        after 5000 -> ct:fail(exec_process_did_not_exit)
        end
    end, [{"TERM", 15}, {"KILL", 9}, {"INT", 2}, {"PIPE", 13}]).

background_holds_output(_Config) ->
    lists:foreach(fun(Extra) ->
        Opts = run_opts(),
        Start = erlang:monotonic_time(millisecond),
        ?assertEqual(#{exit_code => 0, stdout => <<"done">>, stderr => <<"err">>},
            klsn_bwrap:run([<<"/bin/sh">>, <<"-c">>,
                <<"sleep 3 & printf done; printf err >&2">>],
                Opts#{bwrap := maps:get(bwrap, Opts) ++ Extra, timeout := 1500})),
        ?assert(erlang:monotonic_time(millisecond) - Start < 2000)
    end, [[], [new_session]]).

continuous_background_output(_Config) ->
    lists:foreach(fun(Stream) ->
        Opts = run_opts(),
        Command = <<"yes >", Stream/binary, " & sleep 0.05; exit 0">>,
        Start = erlang:monotonic_time(millisecond),
        ?assertMatch(#{exit_code := 0}, klsn_bwrap:run(
            [<<"/bin/sh">>, <<"-c">>, Command],
            Opts#{bwrap := [new_session | maps:get(bwrap, Opts)], timeout := 1500})),
        ?assert(erlang:monotonic_time(millisecond) - Start < 2000)
    end, [<<"/dev/stdout">>, <<"/dev/stderr">>]).

private_temporary_directory(_Config) ->
    Tmp = case os:getenv("TMPDIR") of false -> "/tmp"; Path -> Path end,
    Pattern = filename:join(Tmp, "klsn-bwrap-" ++ os:getpid() ++ "-*"),
    Before = filelib:wildcard(Pattern),
    Handle = klsn_bwrap:open([<<"/bin/cat">>], open_opts()),
    [Dir] = filelib:wildcard(Pattern) -- Before,
    try
        {ok, #file_info{mode = Mode}} = file:read_file_info(Dir),
        ?assertEqual(8#700, Mode band 8#777),
        ok = klsn_bwrap:send_eof(Handle),
        ?assertEqual({normal, <<>>, <<>>}, collect(Handle))
    after
        catch klsn_bwrap:stop(Handle)
    end,
    ?assertEqual(Before, filelib:wildcard(Pattern)).

background_survives_completion(Config) ->
    Dir = ?config(priv_dir, Config),
    Marker = filename:join(Dir, "background-finished"),
    Opts = run_opts(),
    try
        ?assertMatch(#{exit_code := 0}, klsn_bwrap:run(
            [<<"/bin/sh">>, <<"-c">>,
                <<"(sleep 0.2; printf done >\"$1\") & exit 0">>, <<"sh">>,
                unicode:characters_to_binary(Marker)],
            Opts#{bwrap := maps:get(bwrap, Opts) ++
                [{bind, unicode:characters_to_binary(Dir), unicode:characters_to_binary(Dir)}]})),
        wait_until(fun() -> file:read_file(Marker) =:= {ok, <<"done">>} end, 200)
    after
        file:delete(Marker)
    end.

background_reopens_output(Config) ->
    Dir = unicode:characters_to_binary(?config(priv_dir, Config)),
    Opts = run_opts(),
    lists:foreach(fun(Stream) ->
        Gate = filename:join(Dir, <<Stream/binary, "-gate">>),
        Opened = filename:join(Dir, <<Stream/binary, "-opened">>),
        Status = filename:join(Dir, <<Stream/binary, "-status">>),
        try
            ?assertEqual(#{exit_code => 0, stdout => <<>>, stderr => <<>>},
                klsn_bwrap:run([<<"/bin/sh">>, <<"-c">>,
                    <<"(i=0; while [ ! -f \"$1\" ]; do i=$((i+1)); "
                      "[ \"$i\" -lt 200 ] || exit 1; sleep 0.01; done; "
                      "timeout 3 sh -c 'exec 3>\"$1\"; printf opened >\"$2\"; "
                      "printf data >&3' sh \"$2\" \"$3\"; "
                      "printf %s \"$?\" >\"$4\") & exit 0">>,
                    <<"sh">>, Gate, <<"/dev/", Stream/binary>>, Opened, Status],
                    Opts#{bwrap := maps:get(bwrap, Opts) ++ [{bind, Dir, Dir}]})),
            %% Reopen only after run has returned and removed every reader.
            ok = file:write_file(Gate, <<>>),
            wait_until(fun() ->
                case file:read_file(Status) of
                    {ok, <<>>} -> false;
                    {ok, _} -> true;
                    _ -> false
                end
            end, 500),
            ?assertEqual({ok, <<"opened">>}, file:read_file(Opened)),
            ?assertEqual({ok, <<"141">>}, file:read_file(Status))
        after
            [file:delete(Path) || Path <- [Gate, Opened, Status]]
        end
    end, [<<"stdout">>, <<"stderr">>]).

large_output(_Config) ->
    #{exit_code := 0, stdout := Data, stderr := <<>>} = klsn_bwrap:run(
        [<<"/usr/bin/dd">>, <<"if=/dev/zero">>, <<"bs=1M">>, <<"count=64">>, <<"status=none">>],
        run_opts()),
    ?assertEqual(64 * 1024 * 1024, byte_size(Data)),
    ?assert(Data =:= binary:copy(<<0>>, byte_size(Data))).

packet_output(Config) ->
    Tools = filename:join(?config(priv_dir, Config), "packet-tools"),
    Dd = filename:join(Tools, "dd"),
    RealDd = os:find_executable("dd"),
    OriginalPath = os:getenv("PATH"),
    ok = file:make_dir(Tools),
    %% Defer active reads until the command has exited, then return one
    %% real pipe packet per successful read. This forces positive short
    %% reads with unread output, independent of scheduling and batch size.
    ok = file:write_file(Dd,
        <<"#!/bin/sh\n"
          "active=false\n"
          "input=\n"
          "for arg; do\n"
          "  case \"$arg\" in\n"
          "    count=1048576) active=true;;\n"
          "    if=*) input=$arg;;\n"
          "  esac\n"
          "done\n"
          %% The environment snapshot also uses dd, without an input path.
          "[ -n \"$input\" ] || exec \"$KLSN_CT_PACKET_DD\" \"$@\"\n"
          "if \"$active\"; then\n"
          "  printf 'dd: Resource temporarily unavailable\\n' >&2; exit 1\n"
          "fi\n"
          "exec \"$KLSN_CT_PACKET_DD\" \"$input\" bs=65536 count=1 iflag=nonblock status=none\n">>),
    ok = file:change_mode(Dd, 8#755),
    try
        with_environment([{"PATH", Tools ++ ":" ++ OriginalPath},
                {"KLSN_CT_PACKET_DD", RealDd}], fun() ->
            Writer = [unicode:characters_to_binary(RealDd), <<"if=/dev/zero">>,
                <<"bs=7">>, <<"count=2">>, <<"oflag=direct">>, <<"status=none">>],
            Data = binary:copy(<<0>>, 14),
            ?assertEqual(#{exit_code => 0, stdout => Data, stderr => <<>>},
                klsn_bwrap:run(Writer, run_opts())),
            ?assertEqual(#{exit_code => 0, stdout => <<>>, stderr => Data},
                klsn_bwrap:run([<<"/bin/sh">>, <<"-c">>, <<"exec \"$@\" >&2">>,
                    <<"sh">> | Writer], run_opts()))
        end)
    after
        file:delete(Dd),
        file:del_dir(Tools)
    end.

large_input(_Config) ->
    Payload = binary:copy(<<0>>, 64 * 1024 * 1024),
    ?assertEqual(#{exit_code => 0, stdout => <<"67108864\n">>, stderr => <<>>},
        klsn_bwrap:run([<<"/usr/bin/wc">>, <<"-c">>],
            (run_opts())#{stdin => Payload})).

restrictive_inherited_umask(Config) ->
    %% A separate VM inherits the restrictive mask without changing the
    %% test runner. The sandbox must still see that original mask.
    Tmp = filename:join(?config(priv_dir, Config), "restrictive-umask"),
    ok = file:make_dir(Tmp),
    Command = [<<"/bin/sh">>, <<"-c">>, <<"umask; printf out; printf err >&2">>],
    Eval = lists:flatten(io_lib:format(
        "R = klsn_bwrap:run(~p, ~p), "
        "io:format(\"~~p\", [{maps:get(exit_code,R), maps:get(stdout,R), maps:get(stderr,R)}]), "
        "halt().", [Command, run_opts()])),
    Expected = iolist_to_binary(io_lib:format("~p", [{0, <<"0777\nout">>, <<"err">>}])),
    try
        Port = open_port({spawn_executable, os:find_executable("sh")},
            [binary, in, eof, exit_status, hide,
                {env, [{"TMPDIR", Tmp}]},
                {args, ["-c", "umask 0777; exec \"$@\"", "klsn-umask",
                    os:find_executable("erl"), "-noshell", "+S", "2", "-pa",
                    filename:join(code:lib_dir(klsn), "ebin"), "-eval", Eval]}]),
        try
            ?assertEqual({0, Expected}, collect_vm(Port, [], false, undefined))
        after
            catch erlang:port_close(Port)
        end,
        ?assertEqual({ok, []}, file:list_dir(Tmp))
    after
        file:del_dir(Tmp)
    end.

invalid_inherited_locale(_Config) ->
    OldLocale = os:getenv("LC_ALL"),
    try
        lists:foreach(fun(Locale) ->
            Expected = case Locale of
                false ->
                    os:unsetenv("LC_ALL"),
                    #{exit_code => 1, stdout => <<>>, stderr => <<>>};
                _ ->
                    true = os:putenv("LC_ALL", Locale),
                    #{exit_code => 0, stdout => list_to_binary(Locale ++ "\n"), stderr => <<>>}
            end,
            Opts = run_opts(),
            ?assertEqual(#{exit_code => 0, stdout => <<"hello\n">>, stderr => <<>>},
                klsn_bwrap:run([<<"/bin/echo">>, <<"hello">>],
                    Opts#{bwrap := [clearenv | maps:get(bwrap, Opts)]})),
            %% Preserve unset, empty and invalid caller locale settings.
            ?assertEqual(Expected,
                klsn_bwrap:run([<<"/usr/bin/printenv">>, <<"LC_ALL">>], Opts))
        end, [false, "", "klsn_MISSING.UTF-8"])
    after
        case OldLocale of
            false -> os:unsetenv("LC_ALL");
            _ -> os:putenv("LC_ALL", OldLocale)
        end
    end.

inherited_ignored_hup(_Config) ->
    %% Start a separate VM with nohup's signal disposition; altering an
    %% environment variable in this VM cannot reproduce an ignored SIGHUP.
    Command = [<<"/bin/sh">>, <<"-c">>, <<"kill -HUP $$; printf survived">>],
    Eval = lists:flatten(io_lib:format(
        "R = klsn_bwrap:run(~p, ~p), "
        "io:format(\"~~p\", [{maps:get(exit_code,R), maps:get(stdout,R), maps:get(stderr,R)}]), "
        "halt().", [Command, run_opts()])),
    Port = open_port({spawn_executable, os:find_executable("env")},
        [binary, in, eof, exit_status, hide,
            {args, ["--ignore-signal=HUP", "--", "erl", "-noshell", "+S", "2",
                "-pa", filename:join(code:lib_dir(klsn), "ebin"), "-eval", Eval]}]),
    try
        ?assertEqual({0, <<"{129,<<>>,<<>>}">>}, collect_vm(Port, [], false, undefined))
    after
        catch erlang:port_close(Port)
    end.

inherited_ignored_int(_Config) ->
    Command = [<<"/bin/sh">>, <<"-c">>, <<"kill -INT $$; printf continued">>],
    Eval = lists:flatten(io_lib:format(
        "R = klsn_bwrap:run(~p, ~p), "
        "io:format(\"~~p\", [{maps:get(exit_code,R), maps:get(stdout,R), maps:get(stderr,R)}]), "
        "halt().", [Command, run_opts()])),
    Port = open_port({spawn_executable, os:find_executable("erl")},
        [binary, in, eof, exit_status, hide,
            {args, ["-noshell", "+Bi", "+S", "2", "-pa",
                filename:join(code:lib_dir(klsn), "ebin"), "-eval", Eval]}]),
    try
        ?assertEqual({0, <<"{130,<<>>,<<>>}">>}, collect_vm(Port, [], false, undefined))
    after
        catch erlang:port_close(Port)
    end.

inherited_blocked_signals(_Config) ->
    %% Clear the whole mask without changing unrelated ignored signals.
    %% TERM must stop the child promptly, without the five-second KILL path.
    Eval = lists:flatten(io_lib:format(
        "#{stdout := S} = klsn_bwrap:run([<<\"/bin/cat\">>,<<\"/proc/self/status\">>], ~p), "
        "Fields = [string:lexemes(binary_to_list(L), \" \\t\") || L <- binary:split(S, <<10>>, [global])], "
        "[Blk] = [list_to_integer(V,16) || [\"SigBlk:\",V] <- Fields], "
        "[Ign] = [list_to_integer(V,16) || [\"SigIgn:\",V] <- Fields], "
        "H = klsn_bwrap:open([<<\"/bin/sh\">>,<<\"-c\">>,<<\"printf ready; exec sleep 30\">>], ~p), "
        "#{os_pid := P, exec_pid := E} = H, "
        "receive {stdout,P,<<\"ready\">>} -> ok after 5000 -> error(no_ready) end, "
        "T = erlang:monotonic_time(millisecond), ok = klsn_bwrap:stop(H), "
        "R = receive {'DOWN',P,process,E,Reason} -> Reason after 7000 -> exit(E,kill), timeout end, "
        "Fast = erlang:monotonic_time(millisecond) - T < 2000, "
        "io:format(\"~~w\", [{Blk, Ign band 128, R, Fast}]), halt().", [run_opts(), open_opts()])),
    Port = open_port({spawn_executable, os:find_executable("env")},
        [binary, in, eof, exit_status, hide,
            {args, ["--block-signal=TERM,USR2", "--ignore-signal=FPE", "--",
                "erl", "-noshell", "+S", "2", "-pa",
                filename:join(code:lib_dir(klsn), "ebin"), "-eval", Eval]}]),
    try
        ?assertEqual({0, <<"{0,128,{exit_status,15},true}">>},
            collect_vm(Port, [], false, undefined))
    after
        catch erlang:port_close(Port)
    end.

inherited_read_timeout(_Config) ->
    with_environment([{"TMOUT", "0.001"}], fun() ->
        ?assertEqual(#{exit_code => 0, stdout => <<"0.001\n">>, stderr => <<>>},
            klsn_bwrap:run([<<"/bin/sh">>, <<"-c">>,
                <<"sleep 0.1; exec printenv TMOUT">>], run_opts()))
    end).

inherited_control_environment(_Config) ->
    Names = ["input", "child", "request", "pipe", "_", "OLDPWD", "BASH_VERSION",
        "RANDOM", "BASH_SUBSHELL", "BASHPID", "LINENO", "SHLVL", "PS1",
        "BASH_EXECUTION_STRING", "COMP_WORDBREAKS", "FUNCNEST"],
    Values = [case Name of
        "input" -> "space ' \" $() =\n" ++ [26085, 26412, 35486];
        _ -> "42"
    end || Name <- Names],
    Command = [<<"/usr/bin/printenv">> | [list_to_binary(Name) || Name <- Names]],
    Opts = run_opts(),
    with_environment(lists:zip(Names, Values), fun() ->
        ?assertEqual(#{exit_code => 0,
                stdout => unicode:characters_to_binary([[Value, "\n"] || Value <- Values]),
                stderr => <<>>}, klsn_bwrap:run(Command, Opts)),
        ?assertEqual(#{exit_code => 1, stdout => <<>>, stderr => <<>>},
            klsn_bwrap:run(Command, Opts#{bwrap := maps:get(bwrap, Opts) ++ [clearenv]})),
        ?assertEqual(#{exit_code => 1, stdout => <<"override\n">>, stderr => <<>>},
            klsn_bwrap:run([<<"/usr/bin/printenv">>, <<"input">>, <<"_">>],
                Opts#{bwrap := maps:get(bwrap, Opts) ++
                    [{setenv, <<"input">>, <<"override">>}, {unsetenv, <<"_">>}]}))
    end),
    with_environment([{Name, false} || Name <- Names], fun() ->
        ?assertEqual(#{exit_code => 1, stdout => <<>>, stderr => <<>>},
            klsn_bwrap:run(Command, Opts))
    end).

large_inherited_environment(_Config) ->
    %% Cover both a large native environment and a restoration value that
    %% crosses many acknowledged upload chunks, including embedded newlines.
    Value = binary:copy(<<"snapshot\n">>, 12000),
    Changes = [{"input", binary_to_list(Value)} |
        [{"KLSN_CT_ENV_" ++ integer_to_list(N), "value"} || N <- lists:seq(1, 3100)]],
    with_environment(Changes, fun() ->
        %% This checks byte preservation across many helper launches, whose
        %% environment-copying cost varies with test-runner load.
        ?assertEqual(#{exit_code => 0,
                stdout => <<Value/binary, "\nvalue\nvalue\n">>, stderr => <<>>},
            klsn_bwrap:run([<<"/usr/bin/printenv">>, <<"input">>, <<"KLSN_CT_ENV_1">>,
                <<"KLSN_CT_ENV_3100">>], (run_opts())#{timeout => 15000}))
    end).

inherited_native_environment(_Config) ->
    %% Latin-1 native encoding allows values that are not valid UTF-8.
    %% Cover a helper variable, a Bash special and an untouched variable.
    Names = ["input", "_", "KLSN_CT_NATIVE"],
    Command = [<<"/usr/bin/printenv">> | [list_to_binary(Name) || Name <- Names]],
    Eval = lists:flatten(io_lib:format(
        "[os:putenv(N,[255,254,233]) || N <- ~p], R = klsn_bwrap:run(~p, ~p), "
        "io:format(\"~~w\", [{maps:get(exit_code,R), maps:get(stdout,R), maps:get(stderr,R)}]), "
        "halt().", [Names, Command, run_opts()])),
    Expected = iolist_to_binary(io_lib:format("~w",
        [{0, binary:copy(<<255,254,233,10>>, 3), <<>>}])),
    Port = open_port({spawn_executable, os:find_executable("erl")},
        [binary, in, eof, exit_status, hide,
            {args, ["-noshell", "+fnl", "+S", "2", "-pa",
                filename:join(code:lib_dir(klsn), "ebin"), "-eval", Eval]}]),
    try
        ?assertEqual({0, Expected}, collect_vm(Port, [], false, undefined))
    after
        catch erlang:port_close(Port)
    end.

inherited_posix_mode(_Config) ->
    lists:foreach(fun({Name, Other}) ->
        with_environment([{Name, "1"}, {Other, false}], fun() ->
            lists:foreach(fun(Code) ->
                Command = [<<"/bin/sh">>, <<"-c">>, <<"exit ", (integer_to_binary(Code))/binary>>],
                ?assertEqual(#{exit_code => Code, stdout => <<>>, stderr => <<>>},
                    klsn_bwrap:run(Command, run_opts())),
                Reason = case Code of 0 -> normal; _ -> {exit_status, Code bsl 8} end,
                ?assertEqual({Reason, <<>>, <<>>}, collect(klsn_bwrap:open(Command, open_opts())))
            end, [0, 7, 128, 143, 255]),
            ?assertEqual(#{exit_code => 0, stdout => <<"1\n">>, stderr => <<>>},
                klsn_bwrap:run([<<"/usr/bin/printenv">>, list_to_binary(Name)], run_opts()))
        end)
    end, [{"POSIXLY_CORRECT", "POSIX_PEDANTIC"}, {"POSIX_PEDANTIC", "POSIXLY_CORRECT"}]).

inherited_shell_options(_Config) ->
    with_environment([{"SHELLOPTS", "pipefail"}, {"BASHOPTS", "nullglob"},
            {"BASH_ARGV0", "sandbox-name"}], fun() ->
        Pipe = [<<"/bin/bash">>, <<"-c">>, <<"false | true">>],
        Glob = [<<"/bin/bash">>, <<"-c">>,
            <<"set -- /tmp/klsn-does-not-exist-*; printf %s \"$#\"">>],
        Argv0 = [<<"/bin/bash">>, <<"-c">>, <<"printf %s \"$0\"">>],
        Opts = run_opts(),
        ?assertEqual(#{exit_code => 1, stdout => <<>>, stderr => <<>>},
            klsn_bwrap:run(Pipe, Opts)),
        ?assertEqual(#{exit_code => 0, stdout => <<"0">>, stderr => <<>>},
            klsn_bwrap:run(Glob, Opts)),
        ?assertEqual(#{exit_code => 0, stdout => <<"sandbox-name">>, stderr => <<>>},
            klsn_bwrap:run(Argv0, Opts)),
        ?assertEqual(#{exit_code => 0, stdout => <<"pipefail\nnullglob\nsandbox-name\n">>, stderr => <<>>},
            klsn_bwrap:run([<<"/usr/bin/printenv">>, <<"SHELLOPTS">>, <<"BASHOPTS">>,
                <<"BASH_ARGV0">>], Opts)),
        %% Explicit sandbox options override the restored caller defaults.
        ?assertMatch(#{exit_code := 0}, klsn_bwrap:run(Pipe,
            Opts#{bwrap := maps:get(bwrap, Opts) ++ [clearenv]})),
        ?assertMatch(#{exit_code := 0}, klsn_bwrap:run(Pipe,
            Opts#{bwrap := maps:get(bwrap, Opts) ++ [{setenv, <<"SHELLOPTS">>, <<>>}]})),
        ?assertMatch(#{stdout := <<"1">>}, klsn_bwrap:run(Glob,
            Opts#{bwrap := maps:get(bwrap, Opts) ++ [{unsetenv, <<"BASHOPTS">>}]})),
        ?assertMatch(#{stdout := <<"/bin/bash">>}, klsn_bwrap:run(Argv0,
            Opts#{bwrap := maps:get(bwrap, Opts) ++ [{unsetenv, <<"BASH_ARGV0">>}]}))
    end).

inherited_shell_functions(Config) ->
    Tools = filename:join(?config(priv_dir, Config), "function-tools"),
    Tmp = filename:join(?config(priv_dir, Config), "function-tmp"),
    Sh = filename:join(Tools, "sh"),
    OriginalPath = os:getenv("PATH"),
    Printf = "() { builtin printf 'function:'; builtin printf \"$@\"; }",
    %% If a reader imports this formatter, it contaminates captured output.
    Umask = "() { builtin printf 'function-umask'; builtin umask \"$@\"; }",
    Custom = "() { builtin printf 'custom:'; builtin printf \"$@\"; }",
    ok = file:make_dir(Tools),
    ok = file:make_dir(Tmp),
    try
        ok = file:make_symlink(os:find_executable("bash"), Sh),
        with_environment([{"BASH_FUNC_printf%%", Printf}, {"BASH_FUNC_umask%%", Umask},
                {"TMPDIR", Tmp}], fun() ->
            Check = fun() ->
                Opts = run_opts(),
                Command = [<<"/bin/bash">>, <<"-c">>, <<"printf out; printf err >&2">>],
                ?assertEqual(#{exit_code => 0, stdout => <<"function:out">>, stderr => <<"function:err">>},
                    klsn_bwrap:run(Command, Opts)),
                ?assertEqual(#{exit_code => 0,
                        stdout => iolist_to_binary([Printf, "\n", Umask, "\n"]), stderr => <<>>},
                    klsn_bwrap:run([<<"/usr/bin/printenv">>,
                        <<"BASH_FUNC_printf%%">>, <<"BASH_FUNC_umask%%">>], Opts)),
                lists:foreach(fun(Override) ->
                    ?assertEqual(#{exit_code => 0, stdout => <<"out">>, stderr => <<"err">>},
                        klsn_bwrap:run(Command,
                            Opts#{bwrap := maps:get(bwrap, Opts) ++ [Override]}))
                end, [clearenv, {unsetenv, <<"BASH_FUNC_printf%%">>}]),
                ?assertEqual(#{exit_code => 0, stdout => <<"custom:out">>, stderr => <<"custom:err">>},
                    klsn_bwrap:run(Command, Opts#{bwrap := maps:get(bwrap, Opts) ++
                        [{setenv, <<"BASH_FUNC_printf%%">>, list_to_binary(Custom)}]})),
                Handle = klsn_bwrap:open([<<"/bin/bash">>, <<"-c">>,
                    <<"cat; printf done; printf err >&2">>],
                    (open_opts())#{stdin => <<"initial\n">>}),
                #{os_pid := OsPid} = Handle,
                try
                    expect_stdout(OsPid, <<"initial\n">>),
                    ok = klsn_bwrap:send(Handle, <<"second\n">>),
                    expect_stdout(OsPid, <<"second\n">>),
                    ok = klsn_bwrap:send_eof(Handle),
                    ?assertEqual({normal, <<"function:done">>, <<"function:err">>}, collect(Handle))
                after
                    catch klsn_bwrap:stop(Handle)
                end,
                ?assertEqual({ok, []}, file:list_dir(Tmp))
            end,
            %% Cover Bash-as-sh and the host's default sh implementation.
            lists:foreach(fun(Path) ->
                with_environment([{"PATH", Path}], Check)
            end, [Tools ++ ":" ++ OriginalPath, OriginalPath])
        end)
    after
        file:delete(Sh),
        file:del_dir(Tools),
        file:del_dir(Tmp)
    end.

equals_executable_paths(Config) ->
    Dir = filename:join(?config(priv_dir, Config), "tools=review"),
    OriginalPath = os:getenv("PATH"),
    ok = file:make_dir(Dir),
    try
        lists:foreach(fun(Names) ->
            Links = [begin
                Link = filename:join(Dir, Name),
                ok = file:make_symlink(os:find_executable(Name), Link),
                Link
            end || Name <- Names],
            try
                with_environment([{"PATH", Dir ++ ":" ++ OriginalPath}], fun() ->
                    ?assertEqual(#{exit_code => 0, stdout => <<"hello\n">>, stderr => <<>>},
                        klsn_bwrap:run([<<"/bin/echo">>, <<"hello">>], run_opts()))
                end)
            after
                [file:delete(Link) || Link <- Links]
            end
        end, [["bwrap"], ["bwrap", "bash", "sh", "env", "cat", "dd", "mkdir", "kill", "rm", "rmdir"]])
    after
        file:del_dir(Dir)
    end.

relative_executable_paths(Config) ->
    Base = filename:join(?config(priv_dir, Config), "relative-tools"),
    OriginalPath = os:getenv("PATH"),
    Erl = os:find_executable("erl"),
    Ebin = filename:absname(filename:join(code:lib_dir(klsn), "ebin")),
    Opts = (run_opts())#{stdin => <<"payload">>},
    Eval = lists:flatten(io_lib:format(
        "R = klsn_bwrap:run([<<\"/bin/cat\">>], ~p), "
        "io:format(\"~~p\", [{maps:get(exit_code,R), maps:get(stdout,R), maps:get(stderr,R)}]), "
        "halt().", [Opts#{bwrap := maps:get(bwrap, Opts) ++ [{chdir, <<"/">>}]}])),
    Cases = [["bwrap"], ["cat"], ["dd"],
        ["bwrap", "bash", "env", "cat", "dd", "mkdir", "kill", "rm", "rmdir"]],
    ok = file:make_dir(Base),
    try
        lists:foreach(fun({N, Names}) ->
            Relative = "-tools" ++ integer_to_list(N),
            Dir = filename:join(Base, Relative),
            ok = file:make_dir(Dir),
            Links = [begin
                Link = filename:join(Dir, Name),
                ok = file:make_symlink(os:find_executable(Name), Link),
                Link
            end || Name <- Names],
            try
                %% Change only the child VM's cwd, leaving the CT VM alone.
                Port = open_port({spawn_executable, Erl},
                    [binary, in, eof, exit_status, hide, {cd, Base},
                        {env, [{"PATH", Relative ++ ":" ++ OriginalPath}]},
                        {args, ["-noshell", "+S", "2", "-pa", Ebin, "-eval", Eval]}]),
                try
                    ?assertEqual({0, <<"{0,<<\"payload\">>,<<>>}">>},
                        collect_vm(Port, [], false, undefined))
                after
                    catch erlang:port_close(Port)
                end
            after
                [file:delete(Link) || Link <- Links],
                file:del_dir(Dir)
            end
        end, lists:zip(lists:seq(1, length(Cases)), Cases))
    after
        file:del_dir(Base)
    end.

control_pipe_disconnect(Config) ->
    vm_cleanup(Config, halt).

vm_shutdown_cleans_up(Config) ->
    vm_cleanup(Config, stop).

vm_cleanup(Config, Shutdown) ->
    {ok, EvalBody} = file:read_file(filename:join(?config(data_dir, Config),
        "control_loss.eval")),
    {ok, Script} = file:read_file(filename:join(?config(data_dir, Config),
        "control_loss.sh")),
    Erl = filename:absname(os:find_executable("erl")),
    Bash = filename:absname(os:find_executable("bash")),
    Ebin = filename:absname(filename:join(code:lib_dir(klsn), "ebin")),
    lists:foreach(fun(Extra) ->
        Eval = iolist_to_binary([io_lib:format("Extra = ~p, Shutdown = ~p,~n",
            [Extra, Shutdown]), EvalBody]),
        %% Keep even the failing implementation inside a containing PID
        %% namespace. Stop the supervisor while its VM closes the control
        %% pipe, then resume its buffered status response to trigger SIGPIPE.
        %% A caller's function-depth limit must not constrain cleanup.
        Port = open_port({spawn_executable, os:find_executable("bwrap")},
            [binary, in, eof, exit_status, hide,
                {env, [{"FUNCNEST", "1"}]},
                {args, ["--ro-bind", "/", "/", "--dev", "/dev", "--proc", "/proc",
                    "--unshare-pid", "--tmpfs", "/tmp", "--ro-bind", Ebin, Ebin,
                    "--die-with-parent", "--",
                    Bash, "--noprofile", "--norc", "-p", "-c", Script,
                    "control-loss", Erl, Ebin, Eval, atom_to_list(Shutdown)]}]),
        try
            ?assertEqual({0, <<"all recorded processes and private files removed\n">>},
                collect_vm(Port, [], false, undefined))
        after
            catch erlang:port_close(Port)
        end
    end, [[], [new_session], [unshare_pid, new_session]]).

reader_shutdown_cleans_up(Config) ->
    {ok, EvalBody} = file:read_file(filename:join(?config(data_dir, Config),
        "reader_shutdown.eval")),
    {ok, Script} = file:read_file(filename:join(?config(data_dir, Config),
        "reader_shutdown.sh")),
    Erl = filename:absname(os:find_executable("erl")),
    Bash = filename:absname(os:find_executable("bash")),
    Ebin = filename:absname(filename:join(code:lib_dir(klsn), "ebin")),
    lists:foreach(fun(Stream) ->
        Eval = iolist_to_binary([io_lib:format("Stream = ~p,~n", [Stream]), EvalBody]),
        %% Resume a pending reader after unlinking but before rmdir. Its
        %% diagnostic must not recreate a file after the VM has exited.
        Port = open_port({spawn_executable, os:find_executable("bwrap")},
            [binary, in, eof, exit_status, hide,
                {env, [{"FUNCNEST", "1"}]},
                {args, ["--ro-bind", "/", "/", "--dev", "/dev", "--proc", "/proc",
                    "--unshare-pid", "--tmpfs", "/tmp", "--ro-bind", Ebin, Ebin,
                    "--die-with-parent", "--", Bash, "--noprofile", "--norc", "-p", "-c",
                    Script, "reader-shutdown", Erl, Ebin, Eval]}]),
        try
            ?assertEqual({0, <<"late reader exited; all private files removed\n">>},
                collect_vm(Port, [], false, undefined))
        after
            catch erlang:port_close(Port)
        end
    end, [stdout, stderr]).

cwd_changes_after_open(Config) ->
    {ok, EvalBody} = file:read_file(filename:join(?config(data_dir, Config),
        "cwd_change.eval")),
    OriginalPath = os:getenv("PATH"),
    Erl = filename:absname(os:find_executable("erl")),
    Bwrap = filename:absname(os:find_executable("bwrap")),
    Ebin = filename:absname(filename:join(code:lib_dir(klsn), "ebin")),
    Cases = [["bash"], ["dd"], ["kill"], ["rm"], ["rmdir"],
        ["bwrap", "bash", "env", "cat", "dd", "mkdir", "kill", "rm", "rmdir"]],
    lists:foreach(fun({N, Names}) ->
        Base = filename:join(?config(priv_dir, Config), "cwd-change-" ++ integer_to_list(N)),
        Start = filename:join(Base, "start"),
        Moved = filename:join(Base, "moved"),
        After = filename:join(Moved, "deeper"),
        Tools = filename:join(Base, "tools"),
        [ok = file:make_dir(Dir) || Dir <- [Base, Start, Moved, After, Tools]],
        Links = [begin
            Link = filename:join(Tools, Name),
            ok = file:make_symlink(filename:absname(os:find_executable(Name)), Link),
            Link
        end || Name <- Names],
        try
            Eval = iolist_to_binary([io_lib:format("After = ~p,~n", [After]), EvalBody]),
            %% Change the child VM's cwd while its streams remain open.
            %% The relative PATH entries now resolve to a different location.
            Port = open_port({spawn_executable, Bwrap},
                [binary, in, eof, exit_status, hide,
                    {env, [{"PATH", "../tools:" ++ OriginalPath}, {"TMPDIR", "/tmp"}]},
                    {args, ["--ro-bind", "/", "/", "--dev", "/dev", "--proc", "/proc",
                        "--unshare-pid", "--tmpfs", "/tmp", "--ro-bind", Ebin, Ebin,
                        "--bind", Base, Base,
                        "--chdir", Start, "--die-with-parent", "--", Erl,
                        "-noshell", "+S", "2", "-pa", Ebin, "-eval", Eval]}]),
            try
                ?assertEqual({0, <<"streams and cleanup survived cwd change\n">>},
                    collect_vm(Port, [], false, undefined))
            after
                catch erlang:port_close(Port)
            end
        after
            [file:delete(Link) || Link <- Links],
            [file:del_dir(Dir) || Dir <- [Start, After, Moved, Tools, Base]]
        end
    end, lists:zip(lists:seq(1, length(Cases)), Cases)).

startup_cleanup(Config) ->
    lists:foreach(fun({Setup, Shutdown}) ->
        vm_fixture(Config, "startup_shutdown",
            io_lib:format("Shutdown = ~p,~n", [Shutdown]), [atom_to_list(Setup)],
            <<"startup helper and private files removed\n">>)
    end, [{Setup, Shutdown} || Setup <- [mkdir, dd, stdin],
        Shutdown <- [halt, stop, worker]]).

helper_ownership(Config) ->
    vm_fixture(Config, "worker_registration", [], [],
        <<"killed startup worker left no helper or private files\n">>).

startup_timeout(Config) ->
    lists:foreach(fun(Stage) ->
        vm_fixture(Config, "startup_timeout", [], [atom_to_list(Stage)],
            <<"startup deadline and deferred cleanup verified\n">>)
    end, [environment, mkdir, upload, stdin, launcher]).

timeout_during_diagnostic_read(Config) ->
    runtime_cancellation(Config, diagnostic).

timeout_while_draining(Config) ->
    runtime_cancellation(Config, final_timeout).

stop_while_draining(Config) ->
    runtime_cancellation(Config, final_stop).

stop_during_diagnostic_read(Config) ->
    runtime_cancellation(Config, diagnostic_stop).

background_survives_before_wait(Config) ->
    runtime_cancellation(Config, background_before_wait).

runtime_cancellation(Config, Mode) ->
    vm_fixture(Config, "runtime_cancel", [], [atom_to_list(Mode)],
        <<"runtime cancellation and cleanup verified\n">>).

stop_with_blocked_file_server(Config) ->
    file_server_shutdown(Config, stop).

control_exit_with_blocked_file_server(Config) ->
    file_server_shutdown(Config, control_kill).

completion_with_blocked_file_server(Config) ->
    file_server_shutdown(Config, completion).

file_server_shutdown(Config, Mode) ->
    vm_fixture(Config, "file_server_shutdown", io_lib:format("Mode = ~p,~n", [Mode]), [],
        <<"shutdown completed while the file server was suspended\n">>).

vm_fixture(Config, Name, Prefix, Args, Expected) ->
    DataDir = ?config(data_dir, Config),
    {ok, Body} = file:read_file(filename:join(DataDir, Name ++ ".eval")),
    {ok, Script} = file:read_file(filename:join(DataDir, Name ++ ".sh")),
    Eval = iolist_to_binary([Prefix, Body]),
    Erl = filename:absname(os:find_executable("erl")),
    Bash = filename:absname(os:find_executable("bash")),
    Ebin = filename:absname(filename:join(code:lib_dir(klsn), "ebin")),
    %% A containing PID namespace bounds deliberately stopped startup jobs.
    Port = open_port({spawn_executable, os:find_executable("bwrap")},
        [binary, in, eof, exit_status, hide,
            {env, [{"FUNCNEST", "1"}]},
            {args, ["--ro-bind", "/", "/", "--dev", "/dev", "--proc", "/proc",
                "--unshare-pid", "--tmpfs", "/tmp", "--ro-bind", Ebin, Ebin,
                "--die-with-parent", "--", Bash, "--noprofile", "--norc", "-p", "-c",
                Script, Name, Erl, Ebin, Eval | Args]}]),
    try
        ?assertEqual({0, Expected}, collect_vm(Port, [], false, undefined))
    after
        catch erlang:port_close(Port)
    end.

with_environment(Changes, Fun) ->
    Before = [{Name, os:getenv(Name)} || {Name, _} <- Changes],
    try
        set_environment(Changes),
        Fun()
    after
        set_environment(Before)
    end.

set_environment(Changes) ->
    lists:foreach(fun
        ({Name, false}) -> os:unsetenv(Name);
        ({Name, Value}) -> os:putenv(Name, Value)
    end, Changes).

%% Probe ports leave stderr on the test runner's diagnostic stream so VM
%% shutdown messages cannot become part of their exact stdout responses.
collect_vm(_Port, Data, true, Status) when is_integer(Status) ->
    {Status, iolist_to_binary(lists:reverse(Data))};
collect_vm(Port, Data, Eof, Status) ->
    receive
        {Port, {data, Chunk}} -> collect_vm(Port, [Chunk | Data], Eof, Status);
        {Port, eof} -> collect_vm(Port, Data, true, Status);
        {Port, {exit_status, Code}} -> collect_vm(Port, Data, Eof, Code)
    after 15000 ->
        ct:fail(vm_probe_timeout)
    end.

wait_until(Predicate, Attempts) ->
    case Predicate() of
        true -> ok;
        false when Attempts > 0 ->
            timer:sleep(10),
            wait_until(Predicate, Attempts - 1);
        false -> ct:fail(cleanup_did_not_finish)
    end.

open_opts() ->
    #{bwrap => [{ro_bind, <<"/">>, <<"/">>}, {dev, <<"/dev">>},
        {proc, <<"/proc">>}, {tmpfs, <<"/tmp">>}]}.

run_opts() ->
    (open_opts())#{timeout => 5000}.

expect_stdout(_OsPid, <<>>) -> ok;
expect_stdout(OsPid, Expected) ->
    receive
        {stdout, OsPid, Data} ->
            Size = byte_size(Data),
            <<Data:Size/binary, Rest/binary>> = Expected,
            expect_stdout(OsPid, Rest)
    after 5000 ->
        ct:fail({missing_stdout, Expected})
    end.

collect(Handle) -> collect(Handle, [], []).

collect(#{os_pid := OsPid, exec_pid := ExecPid} = Handle, Stdout, Stderr) ->
    receive
        {stdout, OsPid, Data} -> collect(Handle, [Data | Stdout], Stderr);
        {stderr, OsPid, Data} -> collect(Handle, Stdout, [Data | Stderr]);
        {'DOWN', OsPid, process, ExecPid, Reason} ->
            %% All output must precede DOWN; callers need no extra drain.
            receive
                {stdout, OsPid, _} -> ct:fail(stdout_after_down);
                {stderr, OsPid, _} -> ct:fail(stderr_after_down)
            after 0 -> ok
            end,
            {Reason, iolist_to_binary(lists:reverse(Stdout)),
                iolist_to_binary(lists:reverse(Stderr))}
    after 10000 ->
        ct:fail({stream_timeout, Handle})
    end.
