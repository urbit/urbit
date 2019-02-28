source $stdenv/setup

set -ex

cp -r $FAKEZOD ./zod
chmod -R u+rw ./zod

urbit -d ./zod

cleanup () {
  if [ -e ./zod/.vere.lock ]
  then kill $(< ./zod/.vere.lock) || true;
  fi
}

trap cleanup EXIT

urb ./zod -p hood -d '+hood/autoload |'
urb ./zod -p hood -d "+hood/mount %"

rm -r ./zod/home
cp -r $ARVO ./zod/home

# cp -r $ARVO/sys            ./zod/home/
# cp    $ARVO/gen/solid.hoon ./zod/home/gen/
# cp    $ARVO/lib/pill.hoon  ./zod/home/lib/

urb ./zod -p hood       -d "+hood/commit %home"
urb ./zod -P urbit.pill -d '+solid, =dub &'

mv urbit.pill $out
