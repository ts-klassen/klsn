set -eu
export TMPDIR=/tmp
cd /tmp
if "$1" -noshell +S 2 -pa "$2" -eval "$3" >/tmp/file-server-vm-log; then
    cat /tmp/file-server-vm-log
else
    status=$?
    cat /tmp/file-server-vm-log
    exit "$status"
fi
