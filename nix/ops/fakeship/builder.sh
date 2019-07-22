source $stdenv/setup

set -ex

if [ -z "$ARVO" ]
then
    $URBIT -d -F $SHIP -B "$PILL" $out
else
    $URBIT -d -F $SHIP -A "$ARVO" -B "$PILL" $out
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

set +x
