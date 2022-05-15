source $stdenv/setup

set -euo pipefail

ARVO=${ARVO?:ARVO location is unset}

cp -r $src ./pier
chmod -R u+rw ./pier

urbit -d ./pier

cleanup () {
  if [ -f ./pier/.vere.lock ]; then
    kill $(< ./pier/.vere.lock) || true
  fi
}

trap cleanup EXIT

port=$(cat ./pier/.http.ports | grep loopback | tr -s ' ' '\n' | head -n 1)

lensa() {
  # -f elided, this can hit server-side timeouts
  curl -s                                                              \
    --data "{\"source\":{\"dojo\":\"$2\"},\"sink\":{\"app\":\"$1\"}}"  \
    "http://localhost:$port" | xargs printf %s | sed 's/\\n/\n/g'
}

lensf() {
  # -f elided, this can hit server-side timeouts
  d=$(echo $1 | sed 's/\./\//g')
  curl -sJO                                                                   \
    --data "{\"source\":{\"dojo\":\"$2\"},\"sink\":{\"output-pill\":\"$d\"}}" \
    "http://localhost:$port"
}

header "updating %base"

# Update pill strategy to ensure correct staging
lensa hood "+hood/mount /=base="

until [ -d ./pier/base ]; do
  sleep 1
done

# Update :lens, :dojo and dependencies
# FIXME: reduce this list
cp $ARVO/app/lens.hoon   ./pier/base/app/
cp $ARVO/app/dojo.hoon   ./pier/base/app/
cp $ARVO/lib/plume.hoon  ./pier/base/lib/
cp $ARVO/lib/server.hoon ./pier/base/lib/
cp $ARVO/lib/sole.hoon   ./pier/base/lib/
cp $ARVO/lib/xray.hoon   ./pier/base/lib/
cp $ARVO/lib/pprint.hoon ./pier/base/lib/

mkdir -p ./pier/base/mar/lens/

cp $ARVO/mar/lens/*      ./pier/base/mar/lens/

cp $ARVO/sur/lens.hoon   ./pier/base/sur/
cp $ARVO/sur/plum.hoon   ./pier/base/sur/
cp $ARVO/sur/sole.hoon   ./pier/base/sur/
cp $ARVO/sur/xray.hoon   ./pier/base/sur/

# Update +solid and its dependencies
cp $ARVO/lib/pill.hoon   ./pier/base/lib/
cp $ARVO/gen/solid.hoon  ./pier/base/gen/

chmod -R u+rw ./pier/base/

lensa hood "+hood/commit %base"
lensa hood "+hood/unmount %base"

# FIXME: horrible hack to ensure the update is applied first
sleep 10

header "updating %stage"

# Stage new desk for pill contents
lensa hood '+hood/merge %stage our %base'
lensa hood "+hood/mount /=stage="

until [ -d ./pier/stage ]; do
  sleep 1
done

rm -rf ./pier/stage
cp -r $ARVO ./pier/stage
chmod -R u+rw ./pier/stage

lensa hood "+hood/commit %stage"
lensa hood "+hood/unmount %stage"

header "running +solid"

lensf solid.pill '+solid /=stage=/sys, =dub &'
lensa hood '+hood/exit'

stopNest
