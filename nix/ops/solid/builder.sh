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

#  update pill strategy to ensure correct staging
#

herb ./pier -p hood -d "+hood/mount /=home="

#  update :lens, :dojo and dependencies
#
#    XX reduce this list
#
cp $ARVO/app/lens.hoon   ./pier/home/app/      2>/dev/null || true
cp $ARVO/app/dojo.hoon   ./pier/home/app/      2>/dev/null || true
cp $ARVO/lib/base64.hoon ./pier/home/lib/      2>/dev/null || true
cp $ARVO/lib/server.hoon ./pier/home/lib/      2>/dev/null || true
cp $ARVO/lib/sole.hoon   ./pier/home/lib/      2>/dev/null || true
mkdir -p ./pier/home/mar/lens/
cp $ARVO/mar/lens/*      ./pier/home/mar/lens/ 2>/dev/null || true

cp $ARVO/sur/lens.hoon   ./pier/home/sur/      2>/dev/null || true
cp $ARVO/sur/sole.hoon   ./pier/home/sur/      2>/dev/null || true

#  update +solid and its dependencies
#
cp $ARVO/lib/pill.hoon   ./pier/home/lib/      2>/dev/null || true
cp $ARVO/gen/solid.hoon  ./pier/home/gen/      2>/dev/null || true

chmod -R u+rw ./pier/home/

herb ./pier -p hood -d "+hood/commit %home"
herb ./pier -p hood -d "+hood/unmount %home"

#  XX horrible hack to ensure the update is applied first
#
sleep 10

#  stage new desk for pill contents
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
