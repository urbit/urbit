{ lib, stdenvNoCC, cacert, python3, bootFakeShip }:

{ urbit, herb, arvo ? null, pill, ship ? "bus", arguments ? urbit.meta.arguments
, doCheck ? true }:

stdenvNoCC.mkDerivation {
  name = "test-${ship}";

  src = bootFakeShip { inherit urbit herb arvo pill ship; };

  phases = [ "unpackPhase" "buildPhase" "checkPhase" ];

  buildInputs = [ cacert urbit herb python3 ];

  unpackPhase = ''
    cp -R $src ./pier
    chmod -R u+rw ./pier
  '';

  buildPhase = ''
    set -x

    urbit ${lib.concatStringsSep " " arguments} -d ./pier 2> urbit-output

    # Sledge Hammer!
    # See: https://github.com/travis-ci/travis-ci/issues/4704#issuecomment-348435959
    python3 -c $'import os\n[os.set_blocking(i, True) for i in range(3)]\n'

    tail -F urbit-output >&2 &

    tailproc=$!

    cleanup () {
      kill $(cat ./pier/.vere.lock) || true
      kill "$tailproc" 2>/dev/null || true

      set +x
    }

    trap cleanup EXIT

    #  measure initial memory usage
    #
    herb ./pier -d '~&  ~  ~&  %init-mass-start  ~'
    herb ./pier -p hood -d '+hood/mass'
    herb ./pier -d '~&  ~  ~&  %init-mass-end  ~'

    #  run the unit tests
    #
    herb ./pier -d '~&  ~  ~&  %test-unit-start  ~'
    herb ./pier -d '####-test %/tests ~'
    herb ./pier -d '~&  ~  ~&  %test-unit-end  ~'

    #  use the :test app to build all agents, generators, and marks
    #
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

    #  measure memory usage post tests
    #
    herb ./pier -d '~&  ~  ~&  %test-mass-start  ~'
    herb ./pier -p hood -d '+hood/mass'
    herb ./pier -d '~&  ~  ~&  %test-mass-end  ~'

    #  defragment the loom
    #
    herb ./pier -d '~&  ~  ~&  %pack-start  ~'
    herb ./pier -p hood -d '+hood/pack'
    herb ./pier -d '~&  ~  ~&  %pack-end  ~'

    #  reclaim space within arvo
    #
    herb ./pier -d '~&  ~  ~&  %trim-start  ~'
    herb ./pier -p hood -d '+hood/trim'
    herb ./pier -d '~&  ~  ~&  %trim-end  ~'

    #  measure memory usage pre |meld
    #
    herb ./pier -d '~&  ~  ~&  %trim-mass-start  ~'
    herb ./pier -p hood -d '+hood/mass'
    herb ./pier -d '~&  ~  ~&  %trim-mass-end  ~'

    #  globally deduplicate
    #
    herb ./pier -d '~&  ~  ~&  %meld-start  ~'
    herb ./pier -p hood -d '+hood/meld'
    herb ./pier -d '~&  ~  ~&  %meld-end  ~'

    #  measure memory usage post |meld
    #
    herb ./pier -d '~&  ~  ~&  %meld-mass-start  ~'
    herb ./pier -p hood -d '+hood/mass'
    herb ./pier -d '~&  ~  ~&  %meld-mass-end  ~'

    herb ./pier -p hood -d '+hood/exit'

    cleanup

    # Collect output
    cp urbit-output test-output-unit
    cp urbit-output test-output-agents
    cp urbit-output test-output-generators
    cp urbit-output test-output-marks

    sed -i '0,/test-unit-start/d'        test-output-unit
    sed -i '/test-unit-end/,$d'          test-output-unit

    sed -i '0,/test-agents-start/d'      test-output-agents
    sed -i '/test-agents-end/,$d'        test-output-agents

    sed -i '0,/test-generators-start/d'  test-output-generators
    sed -i '/test-generators-end/,$d'    test-output-generators

    sed -i '0,/test-marks-start/d'       test-output-marks
    sed -i '/test-marks-end/,$d'         test-output-marks

    mkdir -p $out

    cp test-output-* $out/
  '';

  checkPhase = ''
    hdr () {
      echo =====$(sed 's/./=/g' <<< "$1")=====
      echo ==== $1 ====
      echo =====$(sed 's/./=/g' <<< "$1")=====
    }

    for f in $(find "$out/" -type f); do
      hdr "$(basename $f)"
      cat "$f"
    done

    fail=0

    for f in $(find "$out/" -type f); do
      if egrep "((FAILED|CRASHED)|(ford|warn):) " $f >/dev/null; then
        if [[ $fail -eq 0 ]]; then
          hdr "Test Failures"
        fi

        echo "ERROR Test failure in $(basename $f)"

        ((fail++))
      fi
    done

    if [[ $fail -eq 0 ]]; then
      hdr "Success"
    fi

    exit "$fail"
  '';

  inherit doCheck;

  # Fix 'bind: operation not permitted' when nix.useSandbox = true on darwin.
  # See https://github.com/NixOS/nix/blob/5f6840fbb49ae5b534423bd8a4360646ee93dbaf/src/libstore/build.cc#L2961
  __darwinAllowLocalNetworking = true;

  meta = { platforms = [ "x86_64-linux" ]; };
}
