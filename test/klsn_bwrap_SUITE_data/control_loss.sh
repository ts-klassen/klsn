set -u
export TMPDIR=/tmp
cd /tmp

"$1" -noshell +S 2 -pa "$2" -eval "$3" >/tmp/control-loss-vm-log 2>&1
vm_status=$?
if [ "$vm_status" != 0 ] || [ ! -s /tmp/control-loss-pids ] || [ ! -s /tmp/control-loss-dir ]; then
    cat /tmp/control-loss-vm-log
    exit 10
fi

while read -r role pid start; do
    [ "$role" != control ] || control=$pid
done </tmp/control-loss-pids
if [ "$4" = halt ]; then
    kill -CONT "$control" || exit 11
fi
private_dir=$(</tmp/control-loss-dir)

# Treat zombies as still present: the supervisor must reap its children.
for ((attempt=0; attempt<500; attempt++)); do
    alive=0
    while read -r role pid start; do
        if { stat=$(<"/proc/$pid/stat"); } 2>/dev/null; then
            read -r -a fields <<<"${stat##*) }"
            # A process can disappear after open, leaving a successful empty read.
            [ "${fields[19]-}" != "$start" ] || alive=$((alive+1))
        fi
    done </tmp/control-loss-pids
    if [ "$alive" = 0 ] && [ ! -e "$private_dir" ]; then
        printf 'all recorded processes and private files removed\n'
        exit 0
    fi
    sleep 0.01
done

while read -r role pid start; do
    if [ -e "/proc/$pid/stat" ]; then
        printf 'still present: %s pid=%s\n' "$role" "$pid"
    fi
done </tmp/control-loss-pids
if [ -e "$private_dir" ]; then
    printf 'private directory remains: %s\n' "$private_dir"
fi
exit 12
