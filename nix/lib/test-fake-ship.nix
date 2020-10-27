{ stdenvNoCC, cacert }:

{ urbit, herb, arvo ? null, pill, ship ? "bus" }:

stdenvNoCC.mkDerivation {
  name = "test-${ship}";

  buildInputs = [ cacert urbit herb ];

  phases = [ "buildPhase" "installPhase " ];

  buildPhase = ''
    set -xeuo pipefail

    if ! [ -f "$SSL_CERT_FILE" ]; then
      header "$SSL_CERT_FILE doesn't exist"
      exit 1
    fi

    ARVO=${if arvo == null then "" else arvo}
    PILL=${pill}
    SHIP=${ship}

    if [ -z "$ARVO" ]; then
      urbit -d -F $SHIP -B $PILL ./pier 2> urbit-output
    else
      urbit -d -F $SHIP -A $ARVO -B $PILL ./pier 2> urbit-output
    fi

    tail -f urbit-output >&2 &
    tailproc=$!

    cleanup () {
      if [ -e ./pier/.vere.lock ]; then
        kill $(< ./pier/.vere.lock) || true
      fi

      kill "$tailproc" || true

      set +x
    }

    trap cleanup EXIT

    herb ./pier -p hood -d '+hood/mass'

    # Run the unit tests and then print scrollback
    herb ./pier -d '~&  ~  ~&  %test-unit-start  ~'
    herb ./pier -d '####-test %/tests'
    herb ./pier -d '~&  ~  ~&  %test-unit-end  ~'

    # Start and run the test app
    herb ./pier -p hood -d '+hood/start %test'

    herb ./pier -d '~&  ~  ~&  %test-agents-start  ~'
    herb ./pier -p test -d '%agents'
    herb ./pier -d '~&  ~  ~&  %test-agents-end  ~'

    herb ./pier -d '~&  ~  ~&  %test-generators-start  ~'
    herb ./pier -p test -d '%generators'
    herb ./pier -d '~&  ~  ~&  %test-generators-end  ~'

    herb ./pier -d '~&  ~  ~&  %test-marks-start  ~'
    herb ./pier -p test -d '%marks'
    herb ./pier -d '~&  ~  ~&  %test-marks-end  ~'

    # Compact the loom, comparing memory use before and after
    herb ./pier -p hood -d '+hood/mass'

    herb ./pier -d '~&  ~  ~&  %pack-start  ~'
    herb ./pier -p hood -d '+hood/pack'
    herb ./pier -d '~&  ~  ~&  %pack-end  ~'

    herb ./pier -p hood -d '+hood/mass'
    herb ./pier -p hood -d '+hood/exit'

    # Collect output
    cp urbit-output test-output-unit
    cp urbit-output test-output-agents
    cp urbit-output test-output-generators
    cp urbit-output test-output-marks

    rm urbit-output

    sed -i '0,/test-unit-start/d'        test-output-unit
    sed -i '/test-unit-end/,$d'          test-output-unit

    sed -i '0,/test-agents-start/d'      test-output-agents
    sed -i '/test-agents-end/,$d'        test-output-agents

    sed -i '0,/test-generators-start/d'  test-output-generators
    sed -i '/test-generators-end/,$d'    test-output-generators

    sed -i '0,/test-marks-start/d'       test-output-marks
    sed -i '/test-marks-end/,$d'         test-output-marks
  '';

  installPhase = ''
    mkdir $out
    cp -r test-output-* $out/
  '';

  meta = { platforms = [ "x86_64-linux" ]; };
}
