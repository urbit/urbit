source $stdenv/setup

set -ex

cp -r $SHIP ./ship
chmod -R u+rw ./ship

urbit -d ./ship 2> urbit-output

tail -f urbit-output >&2 &
tailproc=$!

shutdown () {
  if [ -e ./ship/.vere.lock ]
  then kill $(< ./ship/.vere.lock) || true;
  fi

  kill "$tailproc" || true;
}

trap shutdown EXIT

urb ./ship -p hood -d '+hood/autoload |'
urb ./ship -p hood -d '+hood/mount %'

rm -r ./ship/home
cp -r $ARVO ./ship/home

urb ./ship -p hood -d '+hood/commit %home'

# Start the test app
urb ./ship -p hood -d '+hood/start %test'

# Run the %cores tests
urb ./ship -d '~&  ~  ~&  %start-test-cores  ~'
urb ./ship -p test -d ':-  %cores  /'
urb ./ship -d '~&  %finish-test-cores  ~'

# Run the %renders tests
urb ./ship -d '~&  ~  ~&  %start-test-renders  ~'
urb ./ship -p test -d ':-  %renders  /'
urb ./ship -d '~&  %finish-test-renders  ~'

# Run the test generator
urb ./ship -d '+test, =seed `@uvI`(shaz %reproducible)' |
  tee test-generator-output

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
