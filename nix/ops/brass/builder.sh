source $stdenv/setup

set -ex

cp -r $FAKEZOD ./zod
chmod -R u+rw ./zod

$URBIT -d ./zod

cleanup () {
  if [ -e ./zod/.vere.lock ]
  then kill $(< ./zod/.vere.lock) || true;
  fi
}

trap cleanup EXIT

herb ./zod -p hood -d '+hood/autoload |'
herb ./zod -p hood -d "+hood/mount %"

rm -r ./zod/home
cp -r $ARVO ./zod/home

herb ./zod -p hood       -d "+hood/commit %home"
herb ./zod -P brass.pill -d '+brass'

mv brass.pill $out
