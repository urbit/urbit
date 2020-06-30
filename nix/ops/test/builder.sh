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

# Run the unit tests and then print scrollback
herb ./ship -d '~&  ~  ~&  %test-unit-start  ~'
herb ./ship -d '####-test %/tests'
herb ./ship -d '~&  ~  ~&  %test-unit-end  ~'

# Start and run the test app
herb ./ship -p hood -d '+hood/start %test'

herb ./ship -d '~&  ~  ~&  %test-agents-start  ~'
herb ./ship -p test -d '%agents'
herb ./ship -d '~&  ~  ~&  %test-agents-end  ~'

herb ./ship -d '~&  ~  ~&  %test-generators-start  ~'
herb ./ship -p test -d '%generators'
herb ./ship -d '~&  ~  ~&  %test-generators-end  ~'

herb ./ship -d '~&  ~  ~&  %test-marks-start  ~'
herb ./ship -p test -d '%marks'
herb ./ship -d '~&  ~  ~&  %test-marks-end  ~'

# compact the loom, comparing memory use before and after
herb ./ship -p hood -d '+hood/mass'

herb ./ship -d '~&  ~  ~&  %pack-start  ~'
herb ./ship -p hood -d '+hood/pack'
herb ./ship -d '~&  ~  ~&  %pack-end  ~'

herb ./ship -p hood -d '+hood/mass'

shutdown

# Collect output

cp urbit-output test-output-unit
cp urbit-output test-output-agents
cp urbit-output test-output-generators
cp urbit-output test-output-marks
rm urbit-output

sed -i '0,/test-unit-start/d'  test-output-unit
sed -i '/test-unit-end/,$d'    test-output-unit

sed -i '0,/test-agents-start/d'  test-output-agents
sed -i '/test-agents-end/,$d'    test-output-agents

sed -i '0,/test-generators-start/d'  test-output-generators
sed -i '/test-generators-end/,$d'    test-output-generators

sed -i '0,/test-marks-start/d'  test-output-marks
sed -i '/test-marks-end/,$d'    test-output-marks

mkdir $out

cp -r test-output-* $out/

set +x
