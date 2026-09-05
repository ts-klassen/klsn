set -eu
export TMPDIR=/tmp
cd /tmp
mkdir tools checkpoint
printf '%s\n' "$4" >/tmp/runtime-mode
command -v dd >/tmp/runtime-real-dd
cat >tools/dd <<'DD'
#!/bin/sh
read -r mode </tmp/runtime-mode
read -r real </tmp/runtime-real-dd
active=false
final=false
for arg; do
    case "$arg" in
        count=1048576) active=true;;
        count=4194304) final=true;;
    esac
done
case "$mode" in
final_timeout|final_stop)
    if "$active"; then
        printf 'dd: Resource temporarily unavailable\n' >&2
        exit 1
    fi
    if "$final"; then
        printf '%s\n' "$$" >>/tmp/runtime-final-pids
        attempt=0
        while [ ! -e /tmp/runtime-release ]; do
            attempt=$((attempt+1))
            [ "$attempt" -lt 1500 ] || exit 90
            sleep .01
        done
    fi
    ;;
esac
exec "$real" "$@"
DD
chmod +x tools/dd
if PATH=/tmp/tools:$PATH "$1" -noshell +S 1 -pa "$2" -eval "$3" >/tmp/runtime-vm-log; then
    cat /tmp/runtime-vm-log
else
    status=$?
    cat /tmp/runtime-vm-log
    exit "$status"
fi
