source $stdenv/setup

set -ex

export ASAN_OPTIONS="detect_leaks=1:allow_user_segv_handler=1:log_path=asan.log"

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

if ! [ -f ship/.vere.lock ]
then
    echo "Lockfile missing."
    exit 1
fi

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

herb ./ship -d '~&  ~  ~&  %start-pack  ~'
herb ./ship -p hood -d '+hood/pack'
herb ./ship -d '~&  ~  ~&  %finish-pack  ~'

herb ./ship -p hood -d '+hood/exit' || true

while [ -f ship/.vere.lock ]
do
    echo "... awaiting exit"
    sleep 3
done

if [ -n "$(find . -maxdepth 1 -type f -name 'asan.log.*' -print -quit)" ]
then
    for f in ./asan.log.*
    do
        echo "    $f:"
        cat "$f"
    done
    exit 1
fi

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
