source $stdenv/setup

set -euo pipefail

cp -r $src ./pier
chmod -R u+rw ./pier

urbit -d ./pier

cleanup () {
  if [ -f ./pier/.vere.lock ]; then
    kill $(< ./pier/.vere.lock) || true
  fi
}

trap cleanup EXIT

header "running herb +brass"

herb ./pier -P brass.pill -d '+brass'
herb ./pier -p hood -d '+hood/exit'

stopNest
