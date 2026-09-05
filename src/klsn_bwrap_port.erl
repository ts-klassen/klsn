%% Internal Linux port transport. Stock utilities provide file descriptors and
%% wait status; Erlang owns the input queue, output draining and lifecycle.
%% All command arguments and paths are passed separately through {args, ...}.
-module(klsn_bwrap_port).

-export([start/3, start/4, send/2, stop/1, cancel/2]).
-export([init/3, init_guardian/2]).

-define(READ_SIZE, 4194304).
-define(BATCH_SIZE, 1048576).
-define(DRAIN_LIMIT, 2147483647).
-define(SHELL_ENV, ["SHELLOPTS", "BASHOPTS", "BASH_ARGV0", "BASH_COMPAT",
    "BASH_XTRACEFD", "POSIXLY_CORRECT", "POSIX_PEDANTIC", "TMOUT", "FUNCNEST",
    "input", "child", "request", "pipe"]).
%% Noninteractive Bash initializes these variables even when inherited.
%% Keep this bounded: expanding unchanged environment entries into bwrap
%% options can exceed its argument limit for ordinary large environments.
-define(RESTORE_ENV, ?SHELL_ENV ++ ["LC_ALL", "_", "IFS", "OLDPWD", "PWD",
    "BASH", "BASH_VERSION", "BASH_VERSINFO", "BASH_EXECUTION_STRING",
    "BASH_ARGC", "BASH_ARGV", "BASH_LINENO", "BASH_SOURCE", "BASH_SUBSHELL",
    "BASH_REMATCH", "BASHPID", "BASH_COMMAND", "BASH_ALIASES", "BASH_CMDS", "BASH_MONOSECONDS",
    "COMP_WORDBREAKS", "DIRSTACK",
    "EPOCHSECONDS", "EPOCHREALTIME", "EUID", "FUNCNAME", "GROUPS",
    "HISTCMD", "HOSTTYPE", "HOSTNAME", "MACHTYPE", "OSTYPE", "OPTARG",
    "OPTERR", "OPTIND", "PPID", "RANDOM", "SECONDS", "SRANDOM", "SHLVL", "LINENO", "PIPESTATUS",
    "UID", "PS0", "PS1", "PS2", "PS3", "PS4"]).

-record(reader, {
    path, diagnostic, diagnostic_fd, port = undefined, data = [], eof = false, code = undefined,
    pending = undefined, draining = false, drained = 0, done = false
}).
-record(state, {
    owner, os_pid, os_start, control, stdin, bash, dd, stdout, stderr,
    input = {[], []}, stdin_closed = false,
    waiting = false, status = undefined,
    kill_timer = undefined, children = [], stopping = false
}).

start(Argv, Stdin, CloseStdin) ->
    start(Argv, Stdin, CloseStdin, infinity).

start(Argv, Stdin, CloseStdin, Deadline) ->
    case remaining(Deadline) of
        0 -> {error, timeout};
        _ -> start_before(Argv, Stdin, CloseStdin, Deadline)
    end.

start_before(Argv, Stdin, CloseStdin, Deadline) ->
    Ref = make_ref(),
    {Pid, Monitor} = spawn_monitor(?MODULE, init,
        [self(), Ref, {Argv, Stdin, CloseStdin, Deadline}]),
    receive
        {Ref, Result} ->
            case remaining(Deadline) of
                0 -> cancel_start(Pid, Monitor, Ref, Result);
                _ -> erlang:demonitor(Monitor, [flush]), Result
            end;
        {'DOWN', Monitor, process, Pid, Reason} ->
            case remaining(Deadline) of
                0 -> {error, timeout};
                _ -> {error, Reason}
            end
    after remaining(Deadline) ->
        cancel_start(Pid, Monitor, Ref, undefined)
    end.

cancel_start(Pid, Monitor, Ref, Reply) ->
    %% Cleanup may need to wait for mkdir's ownership result. Return at the
    %% deadline while the guardian and external supervisor retain that work.
    exit(Pid, kill),
    receive {'DOWN', Monitor, process, Pid, _} -> ok end,
    %% The worker's messages precede its monitor DOWN. A startup reply that
    %% raced expiry identifies any output already sent by this invocation.
    Result = receive {Ref, LateReply} -> LateReply after 0 -> Reply end,
    case Result of
        {ok, Pid, OsPid} -> flush_cancelled_output(Pid, OsPid);
        _ -> ok
    end,
    {error, timeout}.

%% Deadline cancellation cannot wait on a worker's synchronous I/O or its
%% cleanup. The guardian retains ownership of every native helper.
cancel(Pid, OsPid) ->
    Monitor = erlang:monitor(process, Pid),
    exit(Pid, kill),
    receive {'DOWN', Monitor, process, Pid, _} -> ok end,
    flush_cancelled_output(Pid, OsPid).

flush_cancelled_output(Pid, OsPid) ->
    receive
        {Stream, OsPid, _} when Stream =:= stdout; Stream =:= stderr ->
            flush_cancelled_output(Pid, OsPid);
        {'DOWN', OsPid, process, Pid, _} -> flush_cancelled_output(Pid, OsPid)
    after 0 -> ok
    end.

remaining(infinity) -> infinity;
remaining(Deadline) -> max(0, Deadline - erlang:monotonic_time(millisecond)).

check_startup_deadline() ->
    case get(startup_deadline) of
        undefined -> ok;
        Deadline ->
            case remaining(Deadline) of 0 -> error(timeout); _ -> ok end
    end.

startup_timeout() ->
    check_startup_deadline(),
    min(5000, remaining(get(startup_deadline))).

send(_Target, <<>>) -> ok;
send(Target, Data) ->
    %% The command may finish between chunks. Late stdin, including EOF,
    %% is discarded even if the worker exits while the request is pending.
    case request(Target, {send, Data}) of
        {error, no_process} -> ok;
        Reply -> Reply
    end.

stop(OsPid) -> request(OsPid, stop).

%% Complete handles address the worker directly. For os_pid-only handles,
%% inspect connected port owners rather than all processes in the VM.
request(Target, Request) ->
    case target_owner(Target) of
        undefined -> {error, no_process};
        Pid ->
            Ref = erlang:monitor(process, Pid),
            Pid ! {request, self(), Ref, Request},
            receive
                {Ref, Reply} -> erlang:demonitor(Ref, [flush]), Reply;
                {'DOWN', Ref, process, Pid, _} -> {error, no_process}
            end
    end.

target_owner(#{os_pid := OsPid, exec_pid := Pid}) ->
    case is_pid(Pid) andalso node(Pid) =:= node() andalso owns(Pid, OsPid) of
        true -> Pid;
        false -> undefined
    end;
target_owner(#{os_pid := OsPid}) -> target_owner(OsPid);
target_owner(OsPid) -> find_owner(OsPid, erlang:ports()).

owns(Pid, OsPid) ->
    case process_info(Pid, dictionary) of
        {dictionary, Dict} -> lists:keyfind(os_pid, 1, Dict) =:= {os_pid, OsPid};
        _ -> false
    end.

find_owner(_OsPid, []) -> undefined;
find_owner(OsPid, [Port | Rest]) ->
    Pid = case erlang:port_info(Port, connected) of
        {connected, Connected} ->
            case process_info(Connected, initial_call) of
                {initial_call, {?MODULE, init, 3}} -> Connected;
                {initial_call, {?MODULE, init_guardian, 2}} ->
                    case process_info(Connected, dictionary) of
                        {dictionary, Dict} -> proplists:get_value(owner, Dict);
                        _ -> undefined
                    end;
                _ -> undefined
            end;
        _ -> undefined
    end,
    case is_pid(Pid) andalso owns(Pid, OsPid) of
        true -> Pid;
        false -> find_owner(OsPid, Rest)
    end.

%% @private
init(Owner, Ref, {Argv, Stdin, CloseStdin, Deadline}) ->
    process_flag(trap_exit, true),
    put(startup_deadline, Deadline),
    Reason = try
        check_startup_deadline(),
        {ok, Cwd} = file:get_cwd(),
        SearchPath = absolute_search_path(Cwd),
        Bash = executable("bash", SearchPath),
        Env = executable("env", SearchPath),
        Cat = executable("cat", SearchPath),
        Dd = executable("dd", SearchPath),
        Mkdir = executable("mkdir", SearchPath),
        Kill = executable("kill", SearchPath),
        Rm = executable("rm", SearchPath),
        Rmdir = executable("rmdir", SearchPath),
        put(kill, Kill),
        Parent = self(),
        {Guardian, GuardianMonitor} = spawn_monitor(?MODULE, init_guardian, [Parent, Kill]),
        put(guardian, Guardian),
        put(guardian_monitor, GuardianMonitor),
        EnvironmentData = environment_snapshot(Env),
        Dir = private_directory(Cwd),
        Output = filename:join(Dir, "stdout.error"),
        Error = filename:join(Dir, "stderr.error"),
        Environment = filename:join(Dir, "environment.args"),
        %% Reset inherited ignored signals before Bash, which cannot do so
        %% itself. Helpers have isolated shell options and locale settings.
        CommandArgv = restore_environment(Argv),
        Launcher = [Bash | bash_args(launcher(), ["klsn-bwrap", Environment | CommandArgv])],
        Control = spawn_port(control, Env,
            %% A bare command name avoids env's assignment parsing even when
            %% PATH resolves Bash inside a directory containing '='.
            signal_options() ++ ["--", filename:basename(Bash) |
                bash_args(supervisor(), ["klsn-wait", Cat, Rm, Rmdir, Dir,
                    Mkdir, Dd, integer_to_list(byte_size(EnvironmentData)) | Launcher])],
            [{line, 4096}, {cd, Cwd}]),
        upload_environment(Control, EnvironmentData),
        await_files(Control),
        Guardian ! {dir, Dir, Rm, Rmdir},
        {os_pid, ControlPid} = erlang:port_info(Control, os_pid),
        Descriptors = "/proc/" ++ integer_to_list(ControlPid) ++ "/fd/",
        %% A larger bounded port queue amortizes retries for bulk stdin.
        %% Opening an anonymous pipe through /proc cannot wait for a FIFO
        %% partner. Keep the supervisor's spare writer until this one is ready.
        In = spawn_port(stdin, Bash,
            bash_args("exec 2>/dev/null; exec 3>\"$2\" || exit 1; "
                "exec 4>&1 1>&3 3>&-; printf 'READY\\n' >&4; exec 4>&-; exec -- \"$1\"",
                ["klsn-stdin", Cat, Descriptors ++ "6"]),
            [{line, 4096}, {busy_limits_port, {524288, 2097152}}]),
        await_input(In),
        check_startup_deadline(),
        true = erlang:port_command(Control, <<"start\n">>),
        OsPid = await_ready(Control, undefined, false),
        check_startup_deadline(),
        %% The launcher waits for this grant after reporting readiness. If
        %% its setup outlasts the deadline, cancellation cannot release exec.
        true = erlang:port_command(In, <<"start\n">>),
        Start = start_time(OsPid),
        get(guardian) ! {child, {OsPid, Start}},
        Queue = queue:from_list(case CloseStdin of
            true -> [{Stdin, none}, {eof, none}];
            false -> [{Stdin, none}]
        end),
        put(os_pid, OsPid),
        erase(startup_deadline),
        Owner ! {Ref, {ok, self(), OsPid}},
        self() ! poll_status,
        loop(#state{owner = Owner, os_pid = OsPid, os_start = Start, control = Control,
            stdin = In, input = Queue, bash = Bash, dd = Dd,
            stdout = #reader{path = Descriptors ++ "3", diagnostic = Output,
                diagnostic_fd = Descriptors ++ "7"},
            stderr = #reader{path = Descriptors ++ "4", diagnostic = Error,
                diagnostic_fd = Descriptors ++ "10"}})
    catch
        _:ErrorReason -> ErrorReason
    after
        cleanup()
    end,
    case get(os_pid) of
        undefined -> Owner ! {Ref, {error, Reason}};
        Pid -> Owner ! {'DOWN', Pid, process, self(), Reason}
    end,
    exit(Reason).

%% Helpers skip startup files and function imports.
bash_args(Script, Args) ->
    ["--noprofile", "--norc", "-p", "-c", Script | Args].

%% Bash retains the distinction between a normal high exit code and a signal,
%% which the spawn port's flattened exit_status cannot represent. Job control
%% keeps SIGINT/SIGQUIT at their normal dispositions for the background child.
%% Only the fixed job description goes over this channel, never user output.
supervisor() ->
    %% Resource ownership starts before mkdir. A separate control reader
    %% detects control loss even while an initialization helper is stopped.
    %% Delay cancellation during mkdir until its ownership result is known;
    %% all later setup jobs can be killed and reaped before removing files.
    supervisor_cleanup() ++ supervisor_setup() ++
    "exec 2>/dev/null; "
    "child=; input=; request=; pipe=([2]=\"$2\" [3]=\"$3\" [4]=\"$4\"); "
    "trap 'cleanup all' EXIT; trap 'request=abort' HUP INT QUIT PIPE TERM; "
    "pipe[12]=$(<\"/proc/$$/stat\"); exec 11<&0; "
    "exec 0< <(trap - EXIT; \"$1\" <&11; "
    "same_process \"$$\" \"${pipe[12]}\" && kill -TERM \"$$\"); pipe[6]=$!; exec 11<&-; "
    "pipe[7]=$(<\"/proc/${pipe[6]}/stat\") || :; "
    "\"$5\" -m 700 -- \"${pipe[4]}\" & input=$!; "
    "pipe[1]=$(<\"/proc/$input/stat\") || :; wait \"$input\"; "
    "while same_process \"$input\" \"${pipe[1]}\"; do "
    "[ \"$request\" != abort ] || kill -CONT \"$input\"; wait \"$input\"; done; "
    "wait \"$input\"; pipe[8]=$?; input=; [ \"${pipe[8]}\" = 0 ] || exit 1; "
    "pipe[5]=owned; [ \"$request\" != abort ] || exit 1; "
    "pipe[9]=$(umask); umask 077; "
    "exec 7>\"${pipe[4]}/stdout.error\" 10>\"${pipe[4]}/stderr.error\" "
    "12>\"${pipe[4]}/environment.args\" 11<&0; umask \"${pipe[9]}\"; "
    "pipe[10]=$7; while ((pipe[10] > 0)); do pipe[11]=4096; "
    "((pipe[10] >= pipe[11])) || pipe[11]=${pipe[10]}; printf \"ENV\\n\"; "
    "run_setup \"$6\" bs=4096 \"count=${pipe[11]}\" iflag=count_bytes status=none >&12; "
    "pipe[10]=$((pipe[10] - pipe[11])); done; "
    "exec 11<&- 12>&-; "
    "trap 'request=abort' HUP INT QUIT PIPE TERM; "
    "exec 3< <(:); pipe=$!; wait \"$pipe\"; exec 8>/proc/self/fd/3; "
    "exec 4< <(:); pipe=$!; wait \"$pipe\"; exec 9>/proc/self/fd/4; "
    "exec 6< <(:); pipe=$!; wait \"$pipe\"; exec 13>/proc/self/fd/6; "
    "[ \"$request\" != abort ] || exit 1; trap 'exit 1' HUP INT QUIT PIPE TERM; "
    "printf \"FILES\\n\"; IFS= read -r request || exit 1; "
    "[ \"$request\" = start ] || exit 1; exec 13>&-; shift 7; "
    "trap 'request=abort' HUP INT QUIT PIPE TERM; "
    "set -m; \"$@\" 0<&6 6<&- 5>&1 & child=$!; set +m; exec 6<&-; "
    "pipe[0]=$(<\"/proc/$child/stat\") || :; "
    "trap 'exit 1' HUP INT QUIT PIPE TERM; [ \"$request\" != abort ] || exit 1; "
    "printf \"PID %s\\n\" \"$child\"; "
    "while IFS= read -r request; do case \"$request\" in "
    "stop|stop\\ *) pipe[13]=${request#stop}; printf \"STOP\\n\";; "
    "status) LC_ALL=C jobs -l; "
    "if read -t 0 -u 3; then printf \"OUT\\n\"; fi; "
    "if read -t 0 -u 4; then printf \"ERR\\n\"; fi; printf \".\\n\";; "
    "wait) wait \"$child\"; request=$?; trap 'cleanup done' EXIT; "
    "printf \"WAIT %s\\n\" \"$request\";; "
    "finish) exit 0;; esac; done".

supervisor_cleanup() ->
    %% Check start times around traversal so PID reuse cannot redirect a
    %% cleanup signal. Kill descendants before their parent: new_session
    %% descendants may belong to other process groups.
    %% Read complete stat files because comm may contain embedded newlines.
    "same_process() { local current; local -a before after; "
    "current=$(<\"/proc/$1/stat\") || return 1; "
    "read -r -a before <<<\"${2##*) }\"; read -r -a after <<<\"${current##*) }\"; "
    "[ -n \"${before[19]-}\" ] && [ \"${before[19]}\" = \"${after[19]-}\" ]; }; "
    "live_process() { local current; local -a fields; "
    "same_process \"$1\" \"$2\" || return 1; "
    "current=$(<\"/proc/$1/stat\") || return 1; "
    "read -r -a fields <<<\"${current##*) }\"; "
    "case \"${fields[0]-}\" in ''|Z|X|x) return 1;; esac; "
    "same_process \"$1\" \"$2\"; }; "
    "kill_tree() { local pid stat; local -a children; "
    "live_process \"$1\" \"$2\" || return; "
    "read -r -a children <\"/proc/$1/task/$1/children\" || :; "
    "for pid in \"${children[@]}\"; do "
    "stat=$(<\"/proc/$pid/stat\") && kill_tree \"$pid\" \"$stat\"; done; "
    "live_process \"$1\" \"$2\" || return; "
    "if [ \"${3-}\" = group ]; then kill -KILL -- \"-$1\" \"$1\"; "
    "else kill -KILL -- \"$1\"; fi; }; "
    %% Explicit stop owns the captured groups beyond their leaders' exits.
    %% Linux retains a process-group ID while any member remains. Reject a
    %% present replacement leader rather than applying an old stop to it.
    "kill_stopped_groups() { local entry pid start current; local -a fields; "
    "for entry in ${pipe[13]-}; do pid=${entry%%:*}; start=${entry#*:}; "
    "if current=$(<\"/proc/$pid/stat\"); then "
    "read -r -a fields <<<\"${current##*) }\"; "
    "[ -z \"${fields[19]-}\" ] || [ \"${fields[19]}\" = \"$start\" ] || continue; fi; "
    "kill -KILL -- \"-$pid\"; done; }; "
    "stop_watch() { if [ -n \"${pipe[6]}\" ]; then "
    "kill_tree \"${pipe[6]}\" \"${pipe[7]}\"; wait \"${pipe[6]}\"; pipe[6]=; fi; }; "
    "cleanup() { trap '' HUP INT QUIT PIPE TERM; "
    "if [ \"$1\" = all ] && [ -n \"$child\" ]; then "
    "kill_tree \"$child\" \"${pipe[0]}\" group; wait \"$child\"; fi; "
    "kill_stopped_groups; "
    "if [ -n \"$input\" ]; then kill_tree \"$input\" \"${pipe[1]}\"; wait \"$input\"; fi; "
    "exec 6<&- 13>&-; stop_watch; if [ \"${pipe[5]}\" = owned ]; then "
    "\"${pipe[2]}\" --force -- \"${pipe[4]}/environment.args\" "
    "\"${pipe[4]}/stdout.error\" \"${pipe[4]}/stderr.error\"; "
    "\"${pipe[3]}\" -- \"${pipe[4]}\"; fi; }; ".

supervisor_setup() ->
    "run_setup() { trap 'request=abort' HUP INT QUIT PIPE TERM; "
    "\"$@\" <&11 & input=$!; pipe[1]=$(<\"/proc/$input/stat\") || :; "
    "trap 'exit 1' HUP INT QUIT PIPE TERM; [ \"$request\" != abort ] || exit 1; "
    "wait \"$input\" || exit 1; input=; }; ".

launcher() ->
    "exec 1>&8 2>&9 3<&- 4<&- 7>&- 8>&- 9>&- 10>&- 6<\"$1\"; shift; "
    "printf 'READY\\n' >&5; exec 5>&-; IFS= read -r request || exit 1; "
    "[ \"$request\" = start ] || exit 1; exec -- \"$@\"".

signal_options() ->
    {ok, Status} = file:read_file("/proc/" ++ os:getpid() ++ "/status"),
    [Hex] = [string:trim(Value) || <<"SigIgn:", Value/binary>> <-
        binary:split(Status, <<"\n">>, [global])],
    Mask = binary_to_integer(Hex, 16),
    Ignored = [integer_to_list(N) || N <- lists:seq(1, byte_size(Hex) * 4),
        Mask band (1 bsl (N - 1)) =/= 0],
    %% env has no unblock-only option. Resetting all signals clears the
    %% complete mask; then restore unrelated ignored dispositions, leaving
    %% the signals caught by the former process manager at their defaults.
    ["--default-signal", "--ignore-signal=" ++ string:join(Ignored, ","),
        "--default-signal=HUP,INT,TERM,PIPE,CHLD"].

%% Bubblewrap restores the caller's defaults before processing user options,
%% so clearenv/setenv/unsetenv retain their normal precedence. Its executable
%% path goes directly to exec, never through env's NAME=VALUE parsing. Keep
%% affected environment values in a private argument file, since Bash can
%% overwrite or drop exported variables such as input, _, and OLDPWD.
restore_environment([Bwrap | Args]) ->
    [Bwrap, "--args", "6" | Args].

environment_snapshot(Env) ->
    %% os:getenv() decodes native bytes, which cannot always be re-encoded
    %% losslessly. Capture the VM's current native environment directly.
    Port = owned_port(environment, Env,
        [binary, in, eof, exit_status, hide, {args, ["-0"]}]),
    Data = try
        environment_data(Port, [], false, false)
    after
        catch erlang:port_close(Port)
    end,
    Native = maps:from_list([begin
        [Name, Value] = binary:split(Entry, <<"=">>),
        {Name, Value}
    end || Entry <- binary:split(Data, <<0>>, [global]), Entry =/= <<>>]),
    Restore = lists:append([case maps:find(list_to_binary(Name), Native) of
        error -> ["--unsetenv", Name];
        {ok, Value} -> ["--setenv", Name, Value]
    end || Name <- ?RESTORE_ENV]),
    iolist_to_binary([[Arg, 0] || Arg <- Restore]).

environment_data(_Port, Data, true, true) ->
    check_startup_deadline(),
    iolist_to_binary(lists:reverse(Data));
environment_data(Port, Data, Eof, Exited) ->
    Guardian = get(guardian),
    Timeout = startup_timeout(),
    receive
        {Port, {data, Chunk}} -> environment_data(Port, [Chunk | Data], Eof, Exited);
        {Port, eof} -> environment_data(Port, Data, true, Exited);
        {Port, {exit_status, 0}} -> environment_data(Port, Data, Eof, true);
        {Port, {exit_status, Code}} -> error({environment_port, Code});
        {'EXIT', Port, Reason} when Reason =/= normal -> error(Reason);
        {'DOWN', _, process, Guardian, Reason} -> error({guardian, Reason})
    after Timeout -> error(environment_timeout)
    end.

helper_environment() ->
    [{"LC_ALL", "C"} | [{Name, false} || Name <- ?SHELL_ENV]].

upload_environment(_Port, <<>>) -> ok;
upload_environment(Port, Data) ->
    Guardian = get(guardian),
    Timeout = startup_timeout(),
    %% One PIPE_BUF-sized chunk at a time keeps both control pipes drainable
    %% when VM shutdown flushes ports, even if an initializer is blocked.
    receive
        {Port, {data, {eol, <<"ENV">>}}} ->
            check_startup_deadline(),
            Size = min(4096, byte_size(Data)),
            <<Chunk:Size/binary, Rest/binary>> = Data,
            true = erlang:port_command(Port, Chunk),
            upload_environment(Port, Rest);
        {Port, {exit_status, Code}} -> error({port_start, Code});
        {'EXIT', Port, Reason} -> error(Reason);
        {'DOWN', _, process, Guardian, Reason} -> error({guardian, Reason})
    after Timeout -> error(timeout)
    end.

await_files(Port) ->
    Guardian = get(guardian),
    Timeout = startup_timeout(),
    receive
        {Port, {data, {eol, <<"FILES">>}}} -> check_startup_deadline();
        {Port, {exit_status, Code}} -> error({port_start, Code});
        {'EXIT', Port, Reason} -> error(Reason);
        {'DOWN', _, process, Guardian, Reason} -> error({guardian, Reason})
    after Timeout -> error(timeout)
    end.

await_input(Port) ->
    Guardian = get(guardian),
    Timeout = startup_timeout(),
    receive
        {Port, {data, {eol, <<"READY">>}}} -> check_startup_deadline();
        {Port, {exit_status, Code}} -> error({input_start, Code});
        {Port, eof} -> error(input_eof);
        {'EXIT', Port, Reason} -> error(Reason);
        {'DOWN', _, process, Guardian, Reason} -> error({guardian, Reason})
    after Timeout -> error(timeout)
    end.

await_ready(_Port, OsPid, true) when is_integer(OsPid) ->
    check_startup_deadline(), OsPid;
await_ready(Port, OsPid, Ready) ->
    Guardian = get(guardian),
    Timeout = startup_timeout(),
    receive
        {Port, {data, {eol, <<"PID ", Pid/binary>>}}} ->
            await_ready(Port, binary_to_integer(Pid), Ready);
        {Port, {data, {eol, <<"READY">>}}} -> await_ready(Port, OsPid, true);
        {Port, {exit_status, Code}} -> error({port_start, Code});
        {'EXIT', Port, Reason} -> error(Reason);
        {'DOWN', _, process, Guardian, Reason} -> error({guardian, Reason})
    after Timeout -> error(timeout)
    end.

loop(#state{status = Status, stopping = true} = State) when is_integer(Status) ->
    complete(State);
loop(#state{status = Status, stdout = #reader{done = true},
        stderr = #reader{done = true}} = State) when is_integer(Status) ->
    complete(State);
loop(State0) ->
    {State, Retry} = flush_input(State0),
    #state{owner = Owner, control = Control, stdin = In, stdout = Out, stderr = Err} = State,
    OutPort = Out#reader.port,
    ErrPort = Err#reader.port,
    Guardian = get(guardian),
    receive
        poll_status when State#state.status =:= undefined,
                State#state.waiting =:= false ->
            true = erlang:port_command(Control, <<"status\n">>),
            loop(State);
        {Control, {data, {eol, Line}}} -> loop(control_line(Line, State));
        {Control, {exit_status, _}} when State#state.status =/= undefined -> loop(State);
        {Control, {exit_status, Code}} -> error({control_port, Code});
        {read, Stream} -> loop(start_read(Stream, State));
        {Port, Message} when Port =:= OutPort; Port =:= ErrPort ->
            Stream = case Port of OutPort -> stdout; _ -> stderr end,
            loop(read_message(Stream, Message, State));
        {diagnostic, Stream, Ref, Result} ->
            loop(diagnostic_result(Stream, Ref, Result, State));
        {In, {exit_status, _}} ->
            loop(discard_input(State));
        {request, From, Ref, {send, Data}} ->
            loop(enqueue(Data, {From, Ref}, State));
        {request, From, Ref, stop} when is_integer(State#state.status) ->
            %% The command has exited, but output readers may still be
            %% pending. Stop collecting and let cleanup cancel those ports.
            From ! {Ref, ok},
            complete(State);
        {request, From, Ref, stop} ->
            Next = begin_stop(State#state{stopping = true}),
            From ! {Ref, ok},
            loop(Next);
        kill_timeout ->
            Guardian ! kill_stopped,
            loop(State#state{kill_timer = undefined});
        {'EXIT', Owner, Reason} ->
            case Reason of
                normal -> normal;
                shutdown -> shutdown;
                _ -> {owner_died, Owner, Reason}
            end;
        {'EXIT', In, _} -> loop(discard_input(State));
        {'EXIT', Port, Reason} when Reason =/= normal,
                (Port =:= Control orelse Port =:= OutPort orelse Port =:= ErrPort) ->
            error(Reason);
        {'DOWN', _, process, Guardian, Reason} -> error({guardian, Reason});
        {'EXIT', Pid, Reason} ->
            case lists:member(Pid, diagnostic_readers()) of
                true -> error({diagnostic_reader, Reason});
                false -> loop(State)
            end;
        _ -> loop(State)
    after Retry -> loop(State)
    end.

complete(State) ->
    cancel_timer(State#state.kill_timer),
    put(completed, true),
    case State#state.status of 0 -> normal; Status -> {exit_status, Status} end.

control_line(<<".">>, #state{waiting = false, status = undefined} = State) ->
    erlang:send_after(5, self(), poll_status),
    State;
control_line(<<".">>, State) -> State;
control_line(<<"STOP">>, #state{stopping = true} = State) ->
    %% The native supervisor has recorded stop ownership before TERM can
    %% create children and exit, even if the VM then loses its control pipe.
    signal_live(State#state.os_pid, State#state.os_start, "TERM"),
    signal_children(State#state.children, "TERM"),
    State;
control_line(<<"OUT">>, State) -> start_read(stdout, State);
control_line(<<"ERR">>, State) -> start_read(stderr, State);
control_line(<<"WAIT ", Code/binary>>, #state{waiting = Kind} = State) ->
    Exit = binary_to_integer(Code),
    Raw = case Kind of
        normal -> Exit bsl 8;
        {signal, Core} -> (Exit - 128) bor Core
    end,
    %% Command completion discards pending input, even if an explicit EOF
    %% is still flushing. Descendants must see EOF independently of output
    %% draining, so terminate the writer instead of just closing its port.
    get(guardian) ! {abort_input, State#state.stdin},
    %% Always perform a read after wait, even if an earlier readiness poll
    %% found no data. A pre-wait reader must also be followed by a fresh read.
    self() ! {read, stdout},
    self() ! {read, stderr},
    discard_input(State#state{status = Raw});
control_line(<<"[", _/binary>> = Line, #state{waiting = false} = State) ->
    %% jobs -l: [job]+ PID status fixed-command-description
    [_Job, _Pid, Status | _] = string:lexemes(binary_to_list(Line), " \t"),
    case Status of
        "Running" -> State;
        "Stopped" -> State;
        _ ->
            Kind = case Status of
                "Done" -> normal;
                "Exit" -> normal;
                _ -> {signal, case binary:match(Line, <<"(core dumped)">>) of
                    nomatch -> 0;
                    _ -> 128
                end}
            end,
            true = erlang:port_command(State#state.control, <<"wait\n">>),
            State#state{waiting = Kind}
    end;
control_line(Line, _) -> error({control_protocol, Line}).

%% A nonblocking read ends independently of any inherited writer handles.
%% While running, batch available bytes to avoid a new process per small read.
%% After wait, use larger bounded batches and stop at EAGAIN or the byte cap
%% even if background writers remain. The supervisor owns the pipe endpoints.
%% Diagnostics are separate because EAGAIN can follow partial output.
start_read(Stream, State) ->
    Reader = reader(Stream, State),
    case Reader of
        #reader{port = undefined, done = false, path = Path, diagnostic_fd = DiagnosticFd} ->
            Draining = State#state.status =/= undefined,
            BatchSize = case Draining of
                true -> ?READ_SIZE;
                false -> ?BATCH_SIZE
            end,
            ReadArgs = ["bs=65536", "count=" ++ integer_to_list(BatchSize),
                "iflag=count_bytes,nonblock"],
            %% Use the supervisor's open file so late readers cannot create
            %% new directory entries while shutdown removes the private files.
            Port = spawn_port(reader, State#state.bash,
                bash_args("umask 077; exec 2>\"$1\"; shift; exec -- \"$@\"",
                    ["klsn-read", DiagnosticFd, State#state.dd,
                        "if=" ++ Path, "status=none" | ReadArgs]), [in]),
            set_reader(Stream, Reader#reader{port = Port, data = [], eof = false,
                code = undefined, draining = Draining}, State);
        _ -> State
    end.

read_message(Stream, Message, State) ->
    Reader = reader(Stream, State),
    Next = case Message of
        {data, Data} -> Reader#reader{data = [Data | Reader#reader.data]};
        eof -> Reader#reader{eof = true};
        {exit_status, Code} -> Reader#reader{code = Code}
    end,
    case Next of
        #reader{eof = true, code = Code1} when is_integer(Code1) ->
            finish_read(Stream, Next, State);
        _ -> set_reader(Stream, Next, State)
    end.

finish_read(Stream, Reader, State) ->
    catch erlang:port_close(Reader#reader.port),
    case Reader#reader.code of
        0 -> finish_read(Stream, Reader, false, State);
        1 ->
            %% Keep this stream occupied until its diagnostic arrives: the
            %% next dd would truncate the same file. Raw I/O avoids the
            %% shared file server; the linked helper keeps the worker's
            %% request loop responsive while the read is pending.
            Parent = self(),
            Ref = make_ref(),
            Path = Reader#reader.diagnostic,
            Pid = spawn_link(fun() ->
                Result = raw_read_file(Path),
                Parent ! {diagnostic, Stream, Ref, Result}
            end),
            put(diagnostic_readers, [Pid | diagnostic_readers()]),
            set_reader(Stream, Reader#reader{pending = {Pid, Ref}}, State);
        Code -> error({output_port, Code})
    end.

diagnostic_result(Stream, Ref, Result, State) ->
    #reader{pending = {Pid, Ref}} = Reader = reader(Stream, State),
    put(diagnostic_readers, lists:delete(Pid, diagnostic_readers())),
    {ok, Diagnostic} = Result,
    case binary:match(Diagnostic, <<": Resource temporarily unavailable\n">>) of
        nomatch -> error({output_port, Diagnostic});
        _ -> finish_read(Stream, Reader#reader{pending = undefined}, true, State)
    end.

finish_read(Stream, Reader, WouldBlock, State) ->
    Data = iolist_to_binary(lists:reverse(Reader#reader.data)),
    forward(State#state.owner, Stream, State#state.os_pid, Data),
    Size = byte_size(Data),
    Drained = Reader#reader.drained + case Reader#reader.draining of
        true -> Size;
        false -> 0
    end,
    %% Packet-mode pipes can return positive short reads with more packets
    %% queued. Only EOF/EAGAIN or the byte cap ends the final drain.
    Done = Reader#reader.draining andalso
        (Size =:= 0 orelse WouldBlock orelse Drained >= ?DRAIN_LIMIT),
    case not Done andalso (Size > 0 orelse State#state.status =/= undefined) of
        true -> self() ! {read, Stream};
        false -> ok
    end,
    set_reader(Stream, Reader#reader{port = undefined, data = [],
        done = Done, drained = Drained}, State).

reader(stdout, State) -> State#state.stdout;
reader(stderr, State) -> State#state.stderr.

set_reader(stdout, Reader, State) -> State#state{stdout = Reader};
set_reader(stderr, Reader, State) -> State#state{stderr = Reader}.

enqueue(_Data, Reply, #state{stdin_closed = true} = State) ->
    input_ack(Reply),
    State;
enqueue(Data, Reply, #state{input = Input} = State) ->
    %% A producer waits until its bytes reach the bounded native queue.
    %% The worker keeps processing status, output and stop while it retries.
    State#state{input = queue:in({Data, Reply}, Input)}.

input_ack(none) -> ok;
input_ack({From, Ref}) -> From ! {Ref, ok}, ok.

discard_input(#state{input = Input} = State) ->
    [input_ack(Reply) || {_Data, Reply} <- queue:to_list(Input)],
    State#state{stdin_closed = true, input = queue:new()}.

flush_input(#state{stdin_closed = true} = State) -> {State, infinity};
flush_input(#state{input = Input, stdin = Port} = State) ->
    case queue:out(Input) of
        {empty, _} -> {State, infinity};
        {{value, {<<>>, Reply}}, Rest} ->
            input_ack(Reply),
            flush_input(State#state{input = Rest});
        {{value, {eof, _}}, _} ->
            %% Asynchronous close flushes the port's pending writes first.
            get(guardian) ! {close_input, Port},
            {discard_input(State), infinity};
        {{value, {Data, Reply}}, Rest} ->
            Size = min(60000, byte_size(Data)),
            <<Chunk:Size/binary, Tail/binary>> = Data,
            try erlang:port_command(Port, Chunk, [nosuspend]) of
                true -> {State#state{input = queue:in_r({Tail, Reply}, Rest)}, 0};
                false -> {State, 1}
            catch
                error:badarg ->
                    {discard_input(State), infinity}
            end
    end.

begin_stop(#state{kill_timer = undefined, status = undefined,
        os_pid = OsPid, os_start = Start} = State) ->
    {Children, Groups} = case live_process(OsPid, Start) of
        true ->
            Found = descendants(OsPid),
            {Found, process_groups([{OsPid, Start} | Found])};
        false -> {[], []}
    end,
    get(guardian) ! {stopping, {OsPid, Start}, Children, Groups},
    Specs = [[" ", integer_to_list(Group), ":", case GroupStart of
        undefined -> "-";
        _ -> GroupStart
    end] || {Group, GroupStart} <- Groups],
    true = erlang:port_command(State#state.control, ["stop", Specs, "\n"]),
    Timer = erlang:send_after(5000, self(), kill_timeout),
    State#state{kill_timer = Timer, children = Children};
begin_stop(State) -> State.

forward(_Owner, _Stream, _OsPid, <<>>) -> ok;
forward(Owner, Stream, OsPid, Data) -> Owner ! {Stream, OsPid, Data}.

spawn_port(Kind, Executable, Args, Options) ->
    owned_port(Kind, Executable,
        [binary, exit_status, eof, hide, {env, helper_environment()}, {args, Args} | Options]).

owned_port(Kind, Executable, Options) ->
    %% The guardian opens and owns the port before replying. Worker death
    %% cannot interrupt a port ownership handoff or leave an untracked helper.
    Guardian = get(guardian),
    Monitor = get(guardian_monitor),
    Timeout = case get(startup_deadline) of
        undefined -> infinity;
        _ -> startup_timeout()
    end,
    Ref = make_ref(),
    Guardian ! {spawn_port, Ref, Kind, Executable, Options},
    receive
        {Ref, {ok, Port}} -> check_startup_deadline(), Port;
        {Ref, {error, Reason}} -> error(Reason);
        {'DOWN', Monitor, process, Guardian, Reason} -> error({guardian, Reason})
    after Timeout -> error(timeout)
    end.

utility(Executable, Args) ->
    Options = [binary, exit_status, eof, hide, stderr_to_stdout,
        {env, helper_environment()}, {args, Args}],
    %% The guardian's cleanup utilities must not request a spawn from itself.
    Port = case get(guardian) of
        undefined -> open_port({spawn_executable, Executable}, Options);
        _ -> owned_port(utility, Executable, Options)
    end,
    Guardian = get(guardian),
    try
        receive
            {Port, {exit_status, Status}} -> Status;
            {'DOWN', _, process, Guardian, Reason} -> error({guardian, Reason})
        after 5000 -> error({utility_timeout, Executable})
        end
    after
        catch erlang:port_close(Port)
    end.

signal(OsPid, Signal) ->
    utility(get(kill), ["-" ++ Signal, "--", "-" ++ integer_to_list(OsPid)]),
    ok.

signal_live(OsPid, Start, Signal) ->
    case live_process(OsPid, Start) of
        true -> signal(OsPid, Signal);
        false -> ok
    end.

cleanup() ->
    %% Links cover abrupt worker death; normal exits must explicitly cancel
    %% outstanding diagnostic calls before reporting public completion.
    lists:foreach(fun(Pid) ->
        Monitor = erlang:monitor(process, Pid),
        exit(Pid, kill),
        receive {'DOWN', Monitor, process, Pid, _} -> ok end
    end, diagnostic_readers()),
    erase(diagnostic_readers),
    case get(guardian) of
        undefined -> ok;
        Guardian ->
            Ref = erlang:monitor(process, Guardian),
            Guardian ! {cleanup, get(completed) =:= true},
            receive {'DOWN', Ref, process, Guardian, _} -> ok end
    end.

diagnostic_readers() ->
    case get(diagnostic_readers) of undefined -> []; Readers -> Readers end.

%% @private
init_guardian(Parent, Kill) ->
    process_flag(trap_exit, true),
    put(owner, Parent),
    put(kill, Kill),
    Monitor = erlang:monitor(process, Parent),
    guardian(Parent, Monitor, Kill, undefined, [], undefined).

guardian(Parent, Monitor, Kill, Dir, Ports, Child) ->
    receive
        {dir, Path, Rm, Rmdir} ->
            guardian(Parent, Monitor, Kill, {Path, Rm, Rmdir}, Ports, Child);
        {spawn_port, Ref, Kind, Executable, Options} ->
            case guardian_port(Kind, Executable, Options) of
                {ok, Entry = {_, Port, _, _}} ->
                    Parent ! {Ref, {ok, Port}},
                    guardian(Parent, Monitor, Kill, Dir, [Entry | Ports], Child);
                {error, Reason} ->
                    Parent ! {Ref, {error, Reason}},
                    guardian(Parent, Monitor, Kill, Dir, Ports, Child)
            end;
        {child, Info} -> guardian(Parent, Monitor, Kill, Dir, Ports, Info);
        {stopping, Root, Children, Groups} ->
            put(stopping, {Root, Children, Groups}),
            guardian(Parent, Monitor, Kill, Dir, Ports, Child);
        kill_stopped ->
            kill_stopped(),
            guardian(Parent, Monitor, Kill, Dir, Ports, Child);
        {close_input, Port} ->
            Port ! {self(), close},
            guardian(Parent, Monitor, Kill, Dir, Ports, Child);
        {abort_input, Port} ->
            case lists:keyfind(Port, 2, Ports) of
                {stdin, Port, OsPid, Start} -> terminate(OsPid, Start);
                _ -> ok
            end,
            catch erlang:port_close(Port),
            guardian(Parent, Monitor, Kill, Dir, Ports, Child);
        {cleanup, Completed} -> release(Kill, Dir, Ports, Child, Completed);
        {'DOWN', Monitor, process, Parent, _} -> release(Kill, Dir, Ports, Child, false);
        Message = {Port, {data, {eol, <<"WAIT ", _/binary>>}}} ->
            %% Record completion before forwarding: the worker may be
            %% blocked in a diagnostic read or cancelled before consuming it.
            %% An exited command's surviving process group must be preserved.
            RemainingChild = case lists:keyfind(Port, 2, Ports) of
                {control, _, _, _} -> undefined;
                _ -> Child
            end,
            Parent ! Message,
            guardian(Parent, Monitor, Kill, Dir, Ports, RemainingChild);
        Message = {Port, {exit_status, _}} ->
            Parent ! Message,
            Remaining = case lists:keyfind(Port, 2, Ports) of
                {Kind, _, _, _} when Kind =:= reader; Kind =:= utility; Kind =:= environment ->
                    lists:keydelete(Port, 2, Ports);
                {control, _, OsPid, Start} ->
                    lists:keyreplace(Port, 2, Ports, {exited_control, Port, OsPid, Start});
                _ -> Ports
            end,
            guardian(Parent, Monitor, Kill, Dir, Remaining, Child);
        Message ->
            Parent ! Message,
            guardian(Parent, Monitor, Kill, Dir, Ports, Child)
    end.

guardian_port(Kind, Executable, Options) ->
    try open_port({spawn_executable, Executable}, Options) of
        Port ->
            %% Raw /proc reads never wait on the VM's file server. The
            %% guardian remains responsible if its worker dies during startup.
            case erlang:port_info(Port, os_pid) of
                {os_pid, OsPid} -> {ok, {Kind, Port, OsPid, start_time(OsPid)}};
                undefined -> {ok, {Kind, Port, undefined, undefined}}
            end
    catch
        _:Reason -> {error, Reason}
    end.

release(Kill, Dir, Ports, Child, Completed) ->
    put(kill, Kill),
    kill_stopped(),
    case {Child, Completed} of
        {{Pid, Start}, false} -> terminate(Pid, Start);
        _ -> ok
    end,
    lists:foreach(fun({Kind, Port, OsPid, Start}) ->
        case Kind of
            exited_control -> ok;
            control ->
                %% The supervisor owns setup jobs as well as the sandbox.
                %% Let its trap reap them before unlinking private resources.
                case Completed of
                    true -> catch erlang:port_command(Port, <<"wait\nfinish\n">>);
                    false -> catch signal_children([{OsPid, Start}], "TERM")
                end,
                receive
                    {Port, {exit_status, _}} -> ok;
                    {'EXIT', Port, _} -> ok
                %% Closing the port also trips its external control monitor.
                %% Do not kill the owner while mkdir's result is pending;
                %% it must retain responsibility for any directory it creates.
                after 5000 -> ok
                end;
            _ -> terminate(OsPid, Start)
        end,
        catch erlang:port_close(Port)
    end, Ports),
    case Dir of
        undefined -> ok;
        {Path, Rm, Rmdir} ->
            %% The supervisor normally removes these files. Retain fallback
            %% cleanup for an uncatchable control exit, without waiting on
            %% the shared file server even when the paths are already gone.
            utility(Rm, ["--force", "--" | [filename:join(Path, Name) ||
                Name <- ["environment.args", "stdout.error", "stderr.error"]]]),
            utility(Rmdir, ["--", Path]),
            ok
    end.

terminate(undefined, _Start) -> ok;
terminate(OsPid, Start) ->
    case live_process(OsPid, Start) of
        true ->
            catch signal_children(descendants(OsPid), "KILL"),
            %% Recheck after traversal. An absent or reaped group leader
            %% does not authorize killing its surviving background group.
            catch signal_live(OsPid, Start, "KILL");
        false -> ok
    end.

live_process(OsPid, Start) ->
    case process_identity(OsPid) of
        {State, Start} when Start =/= undefined,
                State =/= "Z", State =/= "X", State =/= "x" -> true;
        _ -> false
    end.

process_groups(Processes) ->
    lists:usort(lists:filtermap(fun({Pid, Start}) ->
        case process_fields(Pid) of
            [State, _Parent, Group | _] = Fields when Start =/= undefined,
                    State =/= "Z", State =/= "X", State =/= "x" ->
                case lists:nth(20, Fields) =:= Start of
                    true ->
                        GroupPid = list_to_integer(Group),
                        %% Membership alone cannot authorize killing an
                        %% unrelated group that this descendant joined.
                        case lists:keyfind(GroupPid, 1, Processes) of
                            {GroupPid, GroupStart} when GroupStart =/= undefined ->
                                {true, {GroupPid, GroupStart}};
                            _ -> false
                        end;
                    false -> false
                end;
            _ -> false
        end
    end, Processes)).

kill_stopped() ->
    case erase(stopping) of
        undefined -> ok;
        {Root, Children, Groups} ->
            %% TERM handlers may fork after the first traversal. Revisit
            %% still-owned parents and retain groups for reparented children.
            Fresh = lists:usort(Children ++ lists:append([
                descendants(Pid) || {Pid, Start} <- [Root | Children],
                    live_process(Pid, Start)])),
            AllGroups = lists:usort(Groups ++ process_groups([Root | Fresh])),
            signal_children(Fresh ++ [Root], "KILL"),
            lists:foreach(fun({Group, Start}) ->
                case start_time(Group) of
                    undefined -> signal(Group, "KILL");
                    Start -> signal(Group, "KILL");
                    _ -> ok
                end
            end, AllGroups)
    end.

descendants(OsPid) ->
    PidString = integer_to_list(OsPid),
    Path = "/proc/" ++ PidString ++ "/task/" ++ PidString ++ "/children",
    case raw_read_file(Path) of
        {ok, Data} ->
            lists:append([begin
                Pid = list_to_integer(Child),
                [{Pid, start_time(Pid)} | descendants(Pid)]
            end || Child <- string:lexemes(binary_to_list(Data), " \n")]);
        {error, _} -> []
    end.

signal_children(Children, Signal) ->
    Pids = [integer_to_list(Pid) || {Pid, Start} <- Children,
        Start =/= undefined, start_time(Pid) =:= Start],
    case Pids of
        [] -> ok;
        _ -> utility(get(kill), ["-" ++ Signal, "--" | Pids]), ok
    end.

start_time(OsPid) ->
    case process_identity(OsPid) of
        {_, Start} -> Start;
        undefined -> undefined
    end.

process_identity(OsPid) ->
    case process_fields(OsPid) of
        [State | _] = Values -> {State, lists:nth(20, Values)};
        undefined -> undefined
    end.

process_fields(OsPid) ->
    case raw_read_file("/proc/" ++ integer_to_list(OsPid) ++ "/stat") of
        {ok, Stat} ->
            %% comm (field 2) may itself contain spaces and parentheses.
            Fields = lists:last(binary:split(Stat, <<") ">>, [global])),
            case string:lexemes(binary_to_list(Fields), " \n") of
                Values when length(Values) >= 20 -> Values;
                _ -> undefined
            end;
        {error, _} -> undefined
    end.

raw_read_file(Path) ->
    case file:open(Path, [read, raw, binary]) of
        {ok, File} ->
            try raw_read_file(File, [])
            after file:close(File)
            end;
        Error -> Error
    end.

raw_read_file(File, Data) ->
    case file:read(File, 4096) of
        {ok, Chunk} -> raw_read_file(File, [Chunk | Data]);
        eof -> {ok, iolist_to_binary(lists:reverse(Data))};
        Error -> Error
    end.

cancel_timer(undefined) -> ok;
cancel_timer(Timer) -> erlang:cancel_timer(Timer).

absolute_search_path(Cwd) ->
    case os:getenv("PATH") of
        false -> false;
        Path -> string:join([filename:absname(Dir, Cwd) ||
            Dir <- string:split(Path, ":", all)], ":")
    end.

executable(Name, false) -> error({not_found, Name});
executable(Name, SearchPath) ->
    case os:find_executable(Name, SearchPath) of
        false -> error({not_found, Name});
        Path -> Path
    end.

private_directory(Cwd) ->
    Tmp = case os:getenv("TMPDIR") of
        false -> "/tmp";
        Path -> filename:absname(Path, Cwd)
    end,
    Name = "klsn-bwrap-" ++ os:getpid() ++ "-" ++
        integer_to_list(erlang:unique_integer([positive, monotonic])),
    Dir = filename:join(Tmp, Name),
    %% Select only; the external owner performs the exclusive mkdir.
    case file:read_link_info(Dir) of
        {ok, _} -> private_directory(Cwd);
        {error, enoent} -> Dir;
        {error, Reason} -> error(Reason)
    end.
