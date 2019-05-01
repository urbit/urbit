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

# cp -r $ARVO/sys            ./zod/home/
# cp    $ARVO/gen/solid.hoon ./zod/home/gen/
# cp    $ARVO/lib/pill.hoon  ./zod/home/lib/

herb ./zod -p hood       -d "+hood/commit %home"
herb ./zod -P urbit.pill -d '+solid, =dub &'

mv urbit.pill $out

set +x
