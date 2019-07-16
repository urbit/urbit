source $stdenv/setup

set -ex

cp -r $SHIP ./ship
chmod -R u+rw ./ship

$URBIT -d ./ship 2> urbit-output

tail -f urbit-output >&2 &
tailproc=$!

shutdown () {
  if [ -e ./ship/.vere.lock ]
  then kill $(< ./ship/.vere.lock) || true;
  fi

  kill "$tailproc" || true;
}

trap shutdown EXIT

herb ./ship -p hood -d '+hood/mass'

# Start the test app
herb ./ship -p hood -d '+hood/start %test'

# Run the %cores tests
herb ./ship -d '~&  ~  ~&  %start-test-cores  ~'
herb ./ship -p test -d ':-  %cores  /'
herb ./ship -d '~&  %finish-test-cores  ~'

herb ./ship -p hood -d '+hood/mass'

# Run the %renders tests
herb ./ship -d '~&  ~  ~&  %start-test-renders  ~'
herb ./ship -p test -d ':-  %renders  /'
herb ./ship -d '~&  %finish-test-renders  ~'

# Run the test generator
herb ./ship -d '+test, =seed `@uvI`(shaz %reproducible)' |
  tee test-generator-output

herb ./ship -p hood -d '+hood/mass'

shutdown

# Collect output

cp urbit-output test-cores-output
cp urbit-output test-renders-output
rm urbit-output

sed -i '0,/start-test-renders/d'  test-renders-output
sed -i '/finish-test-renders/,$d' test-renders-output

sed -i '0,/start-test-cores/d'  test-cores-output
sed -i '/finish-test-cores/,$d' test-cores-output

mkdir $out

cp test-renders-output   $out/renders
cp test-cores-output     $out/cores
cp test-generator-output $out/generator

set +x
