set -eu
export TMPDIR=/tmp
cd /tmp
mkdir /tmp/stop-checkpoint
if "$1" -noshell +S 2 -pa "$2" -eval "$3" >/tmp/stop-term-vm-log; then
    cat /tmp/stop-term-vm-log
else
    status=$?
    cat /tmp/stop-term-vm-log
    exit "$status"
fi
