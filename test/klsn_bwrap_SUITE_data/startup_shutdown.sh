set -eu
export TMPDIR=/tmp
cd /tmp
mkdir tools
stage=$4
case "$stage" in mkdir|dd) tool=$stage;; stdin) tool=bash;; *) exit 19;; esac
printf '%s\n' "$stage" >/tmp/startup-stage
command -v "$tool" >/tmp/real-startup-helper
cat >"tools/$tool" <<'SETUP'
#!/bin/sh
read -r stage </tmp/startup-stage
read -r real </tmp/real-startup-helper
case "$stage" in
    mkdir)
        "$real" "$@" || exit 20
        for target; do :; done
        private_dir=$target
        ;;
    dd)
        setup=false
        for arg; do [ "$arg" != bs=4096 ] || setup=true; done
        if [ "$setup" != true ]; then exec "$real" "$@"; fi
        target=$(readlink "/proc/$$/fd/1") || exit 21
        private_dir=${target%/*}
        ;;
    stdin)
        if [ "${6-}" != klsn-stdin ]; then exec "$real" "$@"; fi
        target=$8
        control=${target#/proc/}
        control=${control%%/*}
        diagnostic=$(readlink "/proc/$control/fd/7") || exit 22
        private_dir=${diagnostic%/*}
        ;;
esac
printf '%s' "$private_dir" >/tmp/startup-dir
printf '%s\n' "$$" >/tmp/startup-helper
kill -STOP "$$"
if [ "$stage" != mkdir ]; then exec "$real" "$@"; fi
exit 0
SETUP
chmod +x "tools/$tool"
if [ "$stage" = stdin ]; then
    command -v rm >/tmp/real-startup-rm
    cat >tools/rm <<'REMOVE'
#!/bin/sh
read -r real </tmp/real-startup-rm
read -r writer </tmp/startup-helper
printf 'entered\n' >/tmp/startup-remove-checkpoint
# Resume a late writer after its reader is closed but before files vanish.
# A named FIFO could still exist here and trap the writer in its open call.
if kill -CONT "$writer" 2>/dev/null; then
    attempt=0
    while kill -0 "$writer" 2>/dev/null; do
        state=$(cat "/proc/$writer/wchan" 2>/dev/null) || break
        [ "$state" != wait_for_partner ] || break
        attempt=$((attempt+1))
        [ "$attempt" -lt 200 ] || break
        sleep .01
    done
fi
exec "$real" "$@"
REMOVE
    chmod +x tools/rm
fi
if PATH=/tmp/tools:$PATH "$1" -noshell +S 2 -pa "$2" -eval "$3" >/tmp/startup-vm-log 2>&1; then
    status=0
else
    status=$?
fi
if [ "$status" != 0 ] || [ ! -s /tmp/startup-dir ] || [ ! -s /tmp/startup-helper ]; then
    cat /tmp/startup-vm-log
    exit 10
fi
if [ -e /tmp/startup-worker-cleanup ] && [ "$(</tmp/startup-worker-cleanup)" != true ]; then
    printf 'startup helper survived worker death while VM was alive\n'
    cat /tmp/startup-vm-log
    exit 14
fi
private_dir=$(</tmp/startup-dir)
helper=$(</tmp/startup-helper)
for ((attempt=0; attempt<300; attempt++)); do
    if [ ! -e "/proc/$helper/stat" ] && [ ! -e "$private_dir" ]; then
        if [ "$stage" = stdin ] && [ ! -s /tmp/startup-remove-checkpoint ]; then
            printf 'external stdin cleanup checkpoint was not reached\n'
            exit 15
        fi
        printf 'startup helper and private files removed\n'
        exit 0
    fi
    sleep .01
done
[ ! -e "/proc/$helper/stat" ] || printf 'startup helper remains: %s\n' "$helper"
if [ -e "$private_dir" ]; then
    printf 'startup directory remains: %s\n' "$private_dir"
    ls -la "$private_dir"
fi
exit 12
