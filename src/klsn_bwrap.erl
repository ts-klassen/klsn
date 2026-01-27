-module(klsn_bwrap).

-export([
        run/2
    ]).

-export_type([
        command/0
      , opts/0
      , bwrap_opt/0
      , result/0
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

%% Result of run/2.
-type result() :: #{
        exit_code := non_neg_integer()
      , stdout := binary()
      , stderr := binary()
    }.

-spec run(command(), opts()) -> result().
