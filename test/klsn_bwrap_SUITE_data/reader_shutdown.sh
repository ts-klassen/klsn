set -eu
export TMPDIR=/tmp
cd /tmp
mkdir tools
command -v bash >/tmp/real-bash
command -v rm >/tmp/real-rm

# Pause the actual reader immediately before it opens its diagnostic.
cat >tools/bash <<'BASH'
#!/bin/sh
if [ "${6-}" = klsn-read ]; then
    printf '%s\n' "$$" >/tmp/reader-pid
    kill -STOP "$$"
fi
exec "$(cat /tmp/real-bash)" "$@"
BASH
cat >tools/rm <<'RM'
#!/bin/sh
"$(cat /tmp/real-rm)" "$@" || exit 20
if [ -e /tmp/reader-pid ]; then
    read -r reader </tmp/reader-pid
    kill -CONT "$reader" || exit 21
    attempt=0
    # Let the resumed real reader finish before the supervisor tries rmdir.
    # This makes the unlink/open/rmdir interleaving deterministic.
    while kill -0 "$reader" 2>/dev/null; do
        attempt=$((attempt+1))
        [ "$attempt" -lt 500 ] || exit 22
        sleep .01
    done
    printf 'reader exited\n' >/tmp/reader-finished
fi
RM
chmod +x tools/bash tools/rm

if PATH=/tmp/tools:$PATH "$1" -noshell +S 2 -pa "$2" -eval "$3" >/tmp/reader-vm-log 2>&1; then
    vm_status=0
else
    vm_status=$?
fi
if [ "$vm_status" != 0 ] || [ ! -s /tmp/private-dir ] || [ ! -s /tmp/control-pid ]; then
    cat /tmp/reader-vm-log
    exit 10
fi
private_dir=$(</tmp/private-dir)
control=$(</tmp/control-pid)
for ((attempt=0; attempt<500; attempt++)); do
    if [ ! -e "/proc/$control/stat" ]; then
        if [ ! -s /tmp/reader-finished ]; then
            printf 'reader did not finish during external cleanup\n'
            exit 11
        fi
        if [ -e "$private_dir" ]; then
            printf 'private directory remains after supervisor exit: %s\n' "$private_dir"
            ls -la "$private_dir"
            exit 12
        fi
        printf 'late reader exited; all private files removed\n'
        exit 0
    fi
    sleep .01
done
printf 'supervisor still present: %s\n' "$control"
exit 13
