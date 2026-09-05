set -eu
export TMPDIR=/tmp
cd /tmp
mkdir tools command
printf '%s\n' "$4" >/tmp/deadline-stage
for tool in env mkdir dd bash; do
    command -v "$tool" >"/tmp/deadline-real-$tool"
    cat >"tools/$tool" <<'HELPER'
#!/bin/sh
tool=${0##*/}
read -r real <"/tmp/deadline-real-$tool"
read -r stage </tmp/deadline-stage
gate=false
case "$stage:$tool" in
    environment:env) [ "${1-}" != -0 ] || gate=true;;
    mkdir:mkdir) gate=true;;
    upload:dd) [ "${1-}" != bs=4096 ] || gate=true;;
    stdin:bash) [ "${6-}" != klsn-stdin ] || gate=true;;
    launcher:bash) [ "${6-}" != klsn-bwrap ] || gate=true;;
esac
if [ "$gate" = true ]; then
    printf '%s\n' "$$" >/tmp/deadline-helper
    attempt=0
    while [ ! -e /tmp/deadline-release ]; do
        attempt=$((attempt+1))
        [ "$attempt" -lt 1000 ] || exit 90
        sleep .01
    done
fi
exec "$real" "$@"
HELPER
    chmod +x "tools/$tool"
done
if PATH=/tmp/tools:$PATH "$1" -noshell +S 2 -pa "$2" -eval "$3" >/tmp/deadline-vm-log; then
    cat /tmp/deadline-vm-log
else
    status=$?
    cat /tmp/deadline-vm-log
    exit "$status"
fi
