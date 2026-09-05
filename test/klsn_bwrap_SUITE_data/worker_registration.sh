set -eu
export TMPDIR=/tmp
cd /tmp
if "$1" -noshell +S 2 -pa "$2" -eval "$3" >/tmp/registration-vm-log 2>&1; then
    status=0
else
    status=$?
fi
if [ "$status" != 0 ] || [ ! -s /tmp/registration-helper ] || [ ! -s /tmp/registration-vm ]; then
    cat /tmp/registration-vm-log
    exit 10
fi
helper=$(</tmp/registration-helper)
if [ ! -s /tmp/registration-worker-cleanup ] || [ "$(</tmp/registration-worker-cleanup)" != true ]; then
    printf 'helper survived worker death while VM was alive: %s\n' "$helper"
    if [ -e "/proc/$helper/stat" ]; then
        printf 'helper still present after VM shutdown: '
        cat "/proc/$helper/wchan"
        printf '\n'
    fi
    cat /tmp/registration-vm-log
    exit 14
fi
vm=$(</tmp/registration-vm)
shopt -s nullglob
for ((attempt=0; attempt<300; attempt++)); do
    dirs=(/tmp/klsn-bwrap-"$vm"-*)
    if [ ! -e "/proc/$helper/stat" ] && [ "${#dirs[@]}" = 0 ]; then
        printf 'killed startup worker left no helper or private files\n'
        exit 0
    fi
    sleep .01
done
if [ -e "/proc/$helper/stat" ]; then
    printf 'unregistered helper remains: %s\n' "$helper"
    cat "/proc/$helper/wchan"
    printf '\n'
fi
for dir in "${dirs[@]}"; do
    printf 'registration directory remains: %s\n' "$dir"
    ls -la "$dir"
done
cat /tmp/registration-vm-log
exit 12
