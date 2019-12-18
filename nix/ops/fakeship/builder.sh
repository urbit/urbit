source $stdenv/setup

set -ex

export ASAN_OPTIONS="detect_leaks=1:allow_user_segv_handler=1:log_path=asan.log"

if [ -z "$ARVO" ]
then
    $URBIT -d -F $SHIP -B "$PILL" $out
else
    $URBIT -d -F $SHIP -A "$ARVO" -B "$PILL" $out
fi

if ! [ -f $out/.vere.lock ]
then
    echo "Lockfile missing."
    exit 1
fi

check () {
  [ 3 -eq "$(herb $out -d 3)" ]
}

if check
then
    echo "Boot success." >&2
    herb $out -p hood -d '+hood/exit' || true
else
    echo "Boot failure." >&2
    kill $(< $out/.vere.lock) || true
    exit 1
fi

while [ -f $out/.vere.lock ]
do
    echo "... awaiting exit"
    sleep 3
done

if [ -n "$(find . -maxdepth 1 -type f -name 'asan.log.*' -print -quit)" ]
then
    for f in ./asan.log.*
    do
        echo "    $f:"
        cat "$f"
    done
    exit 1
fi

set +x
