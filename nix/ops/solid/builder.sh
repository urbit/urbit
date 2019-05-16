source $stdenv/setup

set -ex

cp -r $PIER ./pier
chmod -R u+rw ./pier

$URBIT -d ./pier

cleanup () {
  if [ -e ./pier/.vere.lock ]
  then kill $(< ./pier/.vere.lock) || true;
  fi
}

trap cleanup EXIT

herb ./pier -p hood -d '+hood/merge %stage our %home'
herb ./pier -p hood -d "+hood/mount /=stage="

rm -r ./pier/stage
cp -r $ARVO ./pier/stage

herb ./pier -p hood -d "+hood/commit %stage"

herb ./pier -P solid.pill -d '+solid /=stage=/sys, =dub &'

mv solid.pill $out

set +x
