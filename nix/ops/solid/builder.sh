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

# update pill strategy to ensure correct staging
#
herb ./pier -p hood -d "+hood/mount /=home="

cp $ARVO/app/lens.hoon ./pier/home/app/
cp $ARVO/lib/pill.hoon ./pier/home/lib/
chmod -R u+rw ./pier/home/lib/

herb ./pier -p hood -d "+hood/commit %home"
herb ./pier -p hood -d "+hood/unmount %home"

# stage new desk for pill contents
#
herb ./pier -p hood -d '+hood/merge %stage our %home'
herb ./pier -p hood -d "+hood/mount /=stage="

rm -rf ./pier/stage
cp -r $ARVO ./pier/stage
chmod -R u+rw ./pier/stage

herb ./pier -p hood -d "+hood/commit %stage"
herb ./pier -p hood -d "+hood/unmount %stage"

herb ./pier -P solid.pill -d '+solid /=stage=/sys, =dub &'

mv solid.pill $out

set +x
