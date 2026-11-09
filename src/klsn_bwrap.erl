-module(klsn_bwrap).

-export([
        run/2
      , open/2
      , send/2
      , send/3
      , send_eof/1
      , send_eof/2
      , stop/1
    ]).

-export_type([
        command/0
      , opts/0
      , open_opts/0
      , bwrap_opt/0
      , result/0
      , stream/0
    ]).

%% argv-style command where each element is a single argument.
-type command() :: [klsn:binstr()].

-type bwrap_opt() ::
        help
      | version
      | {args, non_neg_integer()}
      | unshare_all
      | share_net
      | unshare_user
      | unshare_user_try
      | unshare_ipc
      | unshare_pid
      | unshare_net
      | unshare_uts
      | unshare_cgroup
      | unshare_cgroup_try
      | {userns, non_neg_integer()}
      | {userns2, non_neg_integer()}
      | {pidns, non_neg_integer()}
      | {uid, non_neg_integer()}
      | {gid, non_neg_integer()}
      | {hostname, klsn:binstr()}
      | {chdir, klsn:binstr()}
      | clearenv
      | {setenv, klsn:binstr(), klsn:binstr()}
      | {unsetenv, klsn:binstr()}
      | {lock_file, klsn:binstr()}
      | {sync_fd, non_neg_integer()}
      | {bind, klsn:binstr(), klsn:binstr()}
      | {bind_try, klsn:binstr(), klsn:binstr()}
      | {dev_bind, klsn:binstr(), klsn:binstr()}
      | {dev_bind_try, klsn:binstr(), klsn:binstr()}
      | {ro_bind, klsn:binstr(), klsn:binstr()}
      | {ro_bind_try, klsn:binstr(), klsn:binstr()}
      | {bind_fd, non_neg_integer(), klsn:binstr()}
      | {ro_bind_fd, non_neg_integer(), klsn:binstr()}
      | {remount_ro, klsn:binstr()}
      | {exec_label, klsn:binstr()}
      | {file_label, klsn:binstr()}
      | {proc, klsn:binstr()}
      | {dev, klsn:binstr()}
      | {tmpfs, klsn:binstr()}
      | {mqueue, klsn:binstr()}
      | {dir, klsn:binstr()}
      | {file, non_neg_integer(), klsn:binstr()}
      | {bind_data, non_neg_integer(), klsn:binstr()}
      | {ro_bind_data, non_neg_integer(), klsn:binstr()}
      | {symlink, klsn:binstr(), klsn:binstr()}
      | {seccomp, non_neg_integer()}
      | {add_seccomp, non_neg_integer()}
      | {block_fd, non_neg_integer()}
      | {userns_block_fd, non_neg_integer()}
      | {info_fd, non_neg_integer()}
      | {json_status_fd, non_neg_integer()}
      | new_session
      | die_with_parent
      | as_pid_1
      | {cap_add, klsn:binstr()}
      | {cap_drop, klsn:binstr()}
      | {perms, non_neg_integer()}
      | {chmod, non_neg_integer(), klsn:binstr()}
      .

%% Options for run/2.
%%
%% - bwrap: required list of bubblewrap options
%% - stdin: binary to write then close.
%% - timeout: timeout in milliseconds or infinity
-type opts() :: #{
        bwrap := [bwrap_opt()]
      , stdin => binary() % Unspecified: No stdin
      , timeout => timeout() % Default: infinity
    }.

%% Options for open/2.
%%
%% - bwrap: required list of bubblewrap options
%% - stdin: binary to write without closing stdin
-type open_opts() :: #{
        bwrap := [bwrap_opt()]
      , stdin => binary() % Unspecified: No stdin
    }.

%% Result of run/2.
-type result() :: #{
        exit_code := non_neg_integer()
      , stdout := binary()
      , stderr := binary()
    }.

%% Stream handle returned from open/2.
-type stream() :: #{
        os_pid := integer()
      , exec_pid := pid()
    }.

-spec run(command(), opts()) -> result().
run(Command, Opts) when is_list(Command), is_map(Opts) ->
    BwrapOpts = maps:get(bwrap, Opts),
    Timeout = maps:get(timeout, Opts, infinity),
    Deadline = timeout_deadline(Timeout),

    Argv0 = [
        bwrap_executable()
      | bwrap_opts_to_argv(BwrapOpts)
    ],
    Argv = Argv0 ++ [<<"--">>] ++ Command,

    Stdin = case klsn_map:lookup([stdin], Opts) of
        none ->
            <<>>;
        {value, StdinBinary0} when is_binary(StdinBinary0) ->
            StdinBinary0
      ; {value, _} ->
            erlang:error(badarg, [Command, Opts])
    end,

    case klsn_bwrap_port:start(Argv, Stdin, true, Deadline) of
        {ok, Pid, OsPid} ->
            wait_result(Pid, OsPid, Deadline, [], []);
        {error, Reason} ->
            erlang:error(Reason, [Command, Opts])
    end;
run(Command, Opts) ->
    erlang:error(badarg, [Command, Opts]).

%% @doc
%% Start a bubblewrap sandbox and keep stdin/stdout open for streaming.
%% Output arrives as {stdout | stderr, OsPid, Binary}. Completion arrives as
%% {'DOWN', OsPid, process, ExecPid, normal | {exit_status, WaitStatus}}, where
%% WaitStatus uses the Unix wait-status encoding: normal exit codes are
%% shifted left by 8; termination signals occupy the low 7 bits.
-spec open(command(), open_opts()) -> stream().
open(Command, Opts) when is_list(Command), is_map(Opts) ->
    BwrapOpts = maps:get(bwrap, Opts),

    Argv0 = [
        bwrap_executable()
      | bwrap_opts_to_argv(BwrapOpts)
    ],
    Argv = Argv0 ++ [<<"--">>] ++ Command,

    Stdin = case klsn_map:lookup([stdin], Opts) of
        none ->
            <<>>;
        {value, StdinBinary0} when is_binary(StdinBinary0) ->
            StdinBinary0;
        {value, _} ->
            erlang:error(badarg, [Command, Opts])
    end,

    case klsn_bwrap_port:start(Argv, Stdin, false) of
        {ok, Pid, OsPid} ->
            #{
                os_pid => OsPid
              , exec_pid => Pid
            };
        {error, Reason} ->
            erlang:error(Reason, [Command, Opts])
    end;
open(Command, Opts) ->
    erlang:error(badarg, [Command, Opts]).

%% @doc
%% Send a binary chunk to a streaming sandbox.
%% Waits for capacity in the native stdin buffer. The transport continues
%% collecting output and handling stop while a sender waits for the reader.
%% This wait has no time limit; use send/3 to bound it.
-spec send(stream(), binary()) -> ok.
send(#{os_pid := OsPid} = Handle, Data) when is_integer(OsPid), is_binary(Data) ->
    ok = klsn_bwrap_port:send(Handle, Data);
send(Handle, Data) ->
    erlang:error(badarg, [Handle, Data]).

%% @doc
%% Send a binary chunk, waiting at most Timeout milliseconds (or infinity)
%% for capacity in the native stdin buffer. Empty chunks are a no-op.
%% Like send/2, ok does not guarantee that the sandbox consumed the data;
%% input sent after stdin closes or the command exits is discarded.
%%
%% On expiry, raises timeout after killing the transport and discarding its
%% pending input. The whole stream becomes unusable; open a new stream before
%% sending again. Some or all bytes may already have reached the sandbox.
%% Native process and file cleanup continues asynchronously, and bytes in
%% native buffers may still reach the sandbox. Cancellation does not emit a
%% stream completion message; a separate stream owner can monitor exec_pid
%% to observe transport termination. Previously delivered output and
%% unrelated caller messages are preserved.
-spec send(stream(), binary(), timeout()) -> ok.
send(#{os_pid := OsPid} = Handle, Data, Timeout) when is_integer(OsPid), is_binary(Data),
        (Timeout =:= infinity orelse
            (is_integer(Timeout) andalso Timeout >= 0 andalso Timeout =< 16#ffffffff)) ->
    case klsn_bwrap_port:send(Handle, Data, timeout_deadline(Timeout)) of
        ok -> ok;
        {error, Reason} -> erlang:error(Reason, [Handle, Data, Timeout])
    end;
send(Handle, Data, Timeout) ->
    erlang:error(badarg, [Handle, Data, Timeout]).

%% @doc
%% Close stdin for a streaming sandbox.
%% Waits without a time limit for preceding input; see send_eof/2.
-spec send_eof(stream()) -> ok.
send_eof(#{os_pid := OsPid} = Handle) when is_integer(OsPid) ->
    ok = klsn_bwrap_port:send(Handle, eof);
send_eof(Handle) ->
    erlang:error(badarg, [Handle]).

%% @doc
%% Close stdin after preceding input reaches the native buffer, waiting at
%% most Timeout milliseconds (or infinity). Timeout has the same stream
%% cancellation semantics as send/3. Successful return requests an
%% asynchronous close; it does not wait for the sandbox to observe EOF.
-spec send_eof(stream(), timeout()) -> ok.
send_eof(#{os_pid := OsPid} = Handle, Timeout) when is_integer(OsPid),
        (Timeout =:= infinity orelse
            (is_integer(Timeout) andalso Timeout >= 0 andalso Timeout =< 16#ffffffff)) ->
    case klsn_bwrap_port:send(Handle, eof, timeout_deadline(Timeout)) of
        ok -> ok;
        {error, Reason} -> erlang:error(Reason, [Handle, Timeout])
    end;
send_eof(Handle, Timeout) ->
    erlang:error(badarg, [Handle, Timeout]).

%% @doc
%% Stop a streaming sandbox.
-spec stop(stream()) -> ok.
stop(#{os_pid := OsPid} = Handle) when is_integer(OsPid) ->
    ok = klsn_bwrap_port:stop(Handle);
stop(Handle) ->
    erlang:error(badarg, [Handle]).

wait_result(Pid, OsPid, Deadline, StdoutAcc, StderrAcc) ->
    case timeout_remaining(Deadline) of
        0 -> cancel_run(Pid, OsPid);
        Timeout -> receive_result(Pid, OsPid, Deadline, Timeout, StdoutAcc, StderrAcc)
    end.

receive_result(Pid, OsPid, Deadline, Timeout, StdoutAcc, StderrAcc) ->
    receive
        {stdout, OsPid, Data} when is_binary(Data) ->
            wait_result(Pid, OsPid, Deadline, [StdoutAcc, Data], StderrAcc);
        {stderr, OsPid, Data} when is_binary(Data) ->
            wait_result(Pid, OsPid, Deadline, StdoutAcc, [StderrAcc, Data]);
        {'DOWN', OsPid, process, Pid, {exit_status, ExitStatus}} when is_integer(ExitStatus) ->
            ExitCode = exit_code(ExitStatus),
            {StdoutAcc1, StderrAcc1} = drain_output(OsPid, StdoutAcc, StderrAcc),
            #{
                exit_code => ExitCode
              , stdout => iolist_to_binary(StdoutAcc1)
              , stderr => iolist_to_binary(StderrAcc1)
            };
        {'DOWN', OsPid, process, Pid, normal} ->
            {StdoutAcc1, StderrAcc1} = drain_output(OsPid, StdoutAcc, StderrAcc),
            #{
                exit_code => 0
              , stdout => iolist_to_binary(StdoutAcc1)
              , stderr => iolist_to_binary(StderrAcc1)
            };
        {'DOWN', OsPid, process, Pid, Reason} ->
            erlang:error(Reason, [OsPid])
    after Timeout ->
        cancel_run(Pid, OsPid)
    end.

cancel_run(Pid, OsPid) ->
    ok = klsn_bwrap_port:cancel(Pid, OsPid),
    erlang:error(timeout, [OsPid]).

drain_output(OsPid, StdoutAcc, StderrAcc) ->
    receive
        {stdout, OsPid, Data} when is_binary(Data) ->
            drain_output(OsPid, [StdoutAcc, Data], StderrAcc);
        {stderr, OsPid, Data} when is_binary(Data) ->
            drain_output(OsPid, StdoutAcc, [StderrAcc, Data])
    after 0 ->
        {StdoutAcc, StderrAcc}
    end.

bwrap_executable() ->
    case os:find_executable("bwrap") of
        false ->
            erlang:error(not_found, [bwrap]);
        Path ->
            unicode:characters_to_binary(filename:absname(Path))
    end.

bwrap_opts_to_argv(Opts) when is_list(Opts) ->
    lists:append(lists:map(fun bwrap_opt_to_argv/1, Opts));
bwrap_opts_to_argv(Other) ->
    erlang:error(badarg, [Other]).

%% Strict option parsing: reject unknown atoms/tuples (catch typos).
bwrap_opt_to_argv(help) -> [flag(<<"help">>)];
bwrap_opt_to_argv(version) -> [flag(<<"version">>)];
bwrap_opt_to_argv({args, N}) when is_integer(N), N >= 0 -> [flag(<<"args">>), integer_to_binary(N)];

bwrap_opt_to_argv(unshare_all) -> [flag(<<"unshare-all">>)];
bwrap_opt_to_argv(share_net) -> [flag(<<"share-net">>)];
bwrap_opt_to_argv(unshare_user) -> [flag(<<"unshare-user">>)];
bwrap_opt_to_argv(unshare_user_try) -> [flag(<<"unshare-user-try">>)];
bwrap_opt_to_argv(unshare_ipc) -> [flag(<<"unshare-ipc">>)];
bwrap_opt_to_argv(unshare_pid) -> [flag(<<"unshare-pid">>)];
bwrap_opt_to_argv(unshare_net) -> [flag(<<"unshare-net">>)];
bwrap_opt_to_argv(unshare_uts) -> [flag(<<"unshare-uts">>)];
bwrap_opt_to_argv(unshare_cgroup) -> [flag(<<"unshare-cgroup">>)];
bwrap_opt_to_argv(unshare_cgroup_try) -> [flag(<<"unshare-cgroup-try">>)];

bwrap_opt_to_argv({userns, Fd}) when is_integer(Fd), Fd >= 0 -> [flag(<<"userns">>), integer_to_binary(Fd)];
bwrap_opt_to_argv({userns2, Fd}) when is_integer(Fd), Fd >= 0 -> [flag(<<"userns2">>), integer_to_binary(Fd)];
bwrap_opt_to_argv({pidns, Fd}) when is_integer(Fd), Fd >= 0 -> [flag(<<"pidns">>), integer_to_binary(Fd)];
bwrap_opt_to_argv({uid, Uid}) when is_integer(Uid), Uid >= 0 -> [flag(<<"uid">>), integer_to_binary(Uid)];
bwrap_opt_to_argv({gid, Gid}) when is_integer(Gid), Gid >= 0 -> [flag(<<"gid">>), integer_to_binary(Gid)];

bwrap_opt_to_argv({hostname, Hostname}) when is_binary(Hostname) ->
    [flag(<<"hostname">>), Hostname];
bwrap_opt_to_argv({chdir, Path}) when is_binary(Path) ->
    [flag(<<"chdir">>), Path];

bwrap_opt_to_argv(clearenv) -> [flag(<<"clearenv">>)];
bwrap_opt_to_argv({setenv, Key, Val}) when is_binary(Key), is_binary(Val) ->
    [flag(<<"setenv">>), Key, Val];
bwrap_opt_to_argv({unsetenv, Key}) when is_binary(Key) ->
    [flag(<<"unsetenv">>), Key];
bwrap_opt_to_argv({lock_file, Path}) when is_binary(Path) ->
    [flag(<<"lock-file">>), Path];
bwrap_opt_to_argv({sync_fd, Fd}) when is_integer(Fd), Fd >= 0 ->
    [flag(<<"sync-fd">>), integer_to_binary(Fd)];

bwrap_opt_to_argv({bind, Src, Dst}) when is_binary(Src), is_binary(Dst) ->
    [flag(<<"bind">>), Src, Dst];
bwrap_opt_to_argv({bind_try, Src, Dst}) when is_binary(Src), is_binary(Dst) ->
    [flag(<<"bind-try">>), Src, Dst];
bwrap_opt_to_argv({dev_bind, Src, Dst}) when is_binary(Src), is_binary(Dst) ->
    [flag(<<"dev-bind">>), Src, Dst];
bwrap_opt_to_argv({dev_bind_try, Src, Dst}) when is_binary(Src), is_binary(Dst) ->
    [flag(<<"dev-bind-try">>), Src, Dst];
bwrap_opt_to_argv({ro_bind, Src, Dst}) when is_binary(Src), is_binary(Dst) ->
    [flag(<<"ro-bind">>), Src, Dst];
bwrap_opt_to_argv({ro_bind_try, Src, Dst}) when is_binary(Src), is_binary(Dst) ->
    [flag(<<"ro-bind-try">>), Src, Dst];

bwrap_opt_to_argv({bind_fd, Fd, Path}) when is_integer(Fd), Fd >= 0, is_binary(Path) ->
    [flag(<<"bind-fd">>), integer_to_binary(Fd), Path];
bwrap_opt_to_argv({ro_bind_fd, Fd, Path}) when is_integer(Fd), Fd >= 0, is_binary(Path) ->
    [flag(<<"ro-bind-fd">>), integer_to_binary(Fd), Path];
bwrap_opt_to_argv({remount_ro, Path}) when is_binary(Path) ->
    [flag(<<"remount-ro">>), Path];

bwrap_opt_to_argv({exec_label, Label}) when is_binary(Label) ->
    [flag(<<"exec-label">>), Label];
bwrap_opt_to_argv({file_label, Label}) when is_binary(Label) ->
    [flag(<<"file-label">>), Label];

bwrap_opt_to_argv({proc, Path}) when is_binary(Path) ->
    [flag(<<"proc">>), Path];
bwrap_opt_to_argv({dev, Path}) when is_binary(Path) ->
    [flag(<<"dev">>), Path];
bwrap_opt_to_argv({tmpfs, Path}) when is_binary(Path) ->
    [flag(<<"tmpfs">>), Path];
bwrap_opt_to_argv({mqueue, Path}) when is_binary(Path) ->
    [flag(<<"mqueue">>), Path];
bwrap_opt_to_argv({dir, Path}) when is_binary(Path) ->
    [flag(<<"dir">>), Path];

bwrap_opt_to_argv({file, Fd, Path}) when is_integer(Fd), Fd >= 0, is_binary(Path) ->
    [flag(<<"file">>), integer_to_binary(Fd), Path];
bwrap_opt_to_argv({bind_data, Fd, Path}) when is_integer(Fd), Fd >= 0, is_binary(Path) ->
    [flag(<<"bind-data">>), integer_to_binary(Fd), Path];
bwrap_opt_to_argv({ro_bind_data, Fd, Path}) when is_integer(Fd), Fd >= 0, is_binary(Path) ->
    [flag(<<"ro-bind-data">>), integer_to_binary(Fd), Path];
bwrap_opt_to_argv({symlink, Src, Dst}) when is_binary(Src), is_binary(Dst) ->
    [flag(<<"symlink">>), Src, Dst];

bwrap_opt_to_argv({seccomp, Fd}) when is_integer(Fd), Fd >= 0 ->
    [flag(<<"seccomp">>), integer_to_binary(Fd)];
bwrap_opt_to_argv({add_seccomp, Fd}) when is_integer(Fd), Fd >= 0 ->
    [flag(<<"add-seccomp">>), integer_to_binary(Fd)];
bwrap_opt_to_argv({block_fd, Fd}) when is_integer(Fd), Fd >= 0 ->
    [flag(<<"block-fd">>), integer_to_binary(Fd)];
bwrap_opt_to_argv({userns_block_fd, Fd}) when is_integer(Fd), Fd >= 0 ->
    [flag(<<"userns-block-fd">>), integer_to_binary(Fd)];
bwrap_opt_to_argv({info_fd, Fd}) when is_integer(Fd), Fd >= 0 ->
    [flag(<<"info-fd">>), integer_to_binary(Fd)];
bwrap_opt_to_argv({json_status_fd, Fd}) when is_integer(Fd), Fd >= 0 ->
    [flag(<<"json-status-fd">>), integer_to_binary(Fd)];

bwrap_opt_to_argv(new_session) -> [flag(<<"new-session">>)];
bwrap_opt_to_argv(die_with_parent) -> [flag(<<"die-with-parent">>)];
bwrap_opt_to_argv(as_pid_1) -> [flag(<<"as-pid-1">>)];

bwrap_opt_to_argv({cap_add, Cap}) when is_binary(Cap) ->
    [flag(<<"cap-add">>), Cap];
bwrap_opt_to_argv({cap_drop, Cap}) when is_binary(Cap) ->
    [flag(<<"cap-drop">>), Cap];
bwrap_opt_to_argv({perms, Mode}) when is_integer(Mode), Mode >= 0 ->
    [flag(<<"perms">>), integer_to_binary(Mode)];
bwrap_opt_to_argv({chmod, Mode, Path}) when is_integer(Mode), Mode >= 0, is_binary(Path) ->
    [flag(<<"chmod">>), integer_to_binary(Mode), Path];
bwrap_opt_to_argv(Other) ->
    erlang:error(badarg, [Other]).

flag(Name) when is_binary(Name) ->
    <<"--", Name/binary>>.

timeout_deadline(infinity) ->
    infinity;
timeout_deadline(Timeout) when is_integer(Timeout), Timeout >= 0 ->
    monotonic_ms() + Timeout;
timeout_deadline(Other) ->
    erlang:error(badarg, [Other]).

timeout_remaining(infinity) ->
    infinity;
timeout_remaining(DeadlineMs) when is_integer(DeadlineMs) ->
    Remaining = DeadlineMs - monotonic_ms(),
    case Remaining > 0 of
        true -> Remaining;
        false -> 0
    end.

monotonic_ms() ->
    erlang:convert_time_unit(erlang:monotonic_time(), native, millisecond).

exit_code(WaitStatus) ->
    case WaitStatus band 16#7f of
        0 -> (WaitStatus bsr 8) band 16#ff;
        Signal -> 128 + Signal
    end.
