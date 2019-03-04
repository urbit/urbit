source $stdenv/setup

set -ex

urbit -d -F $SHIP -B "$PILL" $out

check () {
  [ 3 -eq "$(urb $out -d 3)" ]
}

if check
then
    echo "Boot success." >&2
    urb $out -p hood -d '+hood/exit' || true
else
    echo "Boot failure." >&2
    kill $(< $out/.vere.lock) || true
    exit 1
fi
