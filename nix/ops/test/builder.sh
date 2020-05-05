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

# Start the :test app
herb ./ship -p hood -d '+hood/start %test'

# Run the %agents tests
herb ./ship -d '~&  ~  ~&  %start-test-agents  ~'
herb ./ship -p test -d '%agents'
herb ./ship -d '~&  %finish-test-agents  ~'

herb ./ship -p hood -d '+hood/mass'

# Run the %marks tests
herb ./ship -d '~&  ~  ~&  %start-test-marks  ~'
herb ./ship -p test -d '%marks'
herb ./ship -d '~&  %finish-test-marks  ~'

# Run the -test thread
herb ./ship -d '-test /' | tee test-thread-output

herb ./ship -p hood -d '+hood/mass'

herb ./ship -d '~&  ~  ~&  %start-pack  ~'
herb ./ship -p hood -d '+hood/pack'
herb ./ship -d '~&  ~  ~&  %finish-pack  ~'

shutdown

# Collect output

cp urbit-output test-agents-output
cp urbit-output test-marks-output
rm urbit-output

sed -i '0,/start-test-agents/d'  test-agents-output
sed -i '/finish-test-agents/,$d' test-agents-output

sed -i '0,/start-test-marks/d'  test-marks-output
sed -i '/finish-test-marks/,$d' test-marks-output

mkdir $out

cp test-agents-output   $out/agents
cp test-marks-output    $out/marks
cp test-thread-output   $out/tests

set +x
