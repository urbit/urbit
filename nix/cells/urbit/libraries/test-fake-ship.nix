{ lib, stdenvNoCC, curl, python3, bootFakeShip }:

{ urbit, arvo ? null, pill, ship ? "bus", arguments ? urbit.meta.arguments
, doCheck ? true }:

stdenvNoCC.mkDerivation {
  name = "test-${ship}";

  src = bootFakeShip { inherit urbit arvo pill ship; };

  phases = [ "unpackPhase" "buildPhase" "checkPhase" ];

  buildInputs = [ curl python3 urbit ];

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

    port=$(cat ./pier/.http.ports | grep loopback | tr -s ' ' '\n' | head -n 1)

    lensd() {
      # -f elided, this can hit server-side timeouts
      curl -s                                                              \
        --data "{\"source\":{\"dojo\":\"$1\"},\"sink\":{\"stdout\":null}}" \
        "http://localhost:$port" | xargs printf %s | sed 's/\\n/\n/g'
    }

    lensa() {
      # -f elided, this can hit server-side timeouts
      curl -s                                                              \
        --data "{\"source\":{\"dojo\":\"$2\"},\"sink\":{\"app\":\"$1\"}}"  \
        "http://localhost:$port" | xargs printf %s | sed 's/\\n/\n/g'
    }

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
    lensd '~&  ~  ~&  %init-mass-start  ~'
    lensa hood '+hood/mass'
    lensd '~&  ~  ~&  %init-mass-end  ~'

    #  run the unit tests
    #
    lensd '~&  ~  ~&  %test-unit-start  ~'
    lensd '-test %/tests ~'
    lensd '~&  ~  ~&  %test-unit-end  ~'

    #  use the :test app to build all agents, generators, and marks
    #
    lensa hood '+hood/start %test'

    lensd '~&  ~  ~&  %test-agents-start  ~'
    lensa test '%agents'
    lensd '~&  ~  ~&  %test-agents-end  ~'

    lensd '~&  ~  ~&  %test-generators-start  ~'
    lensa test '%generators'
    lensd '~&  ~  ~&  %test-generators-end  ~'

    lensd '~&  ~  ~&  %test-marks-start  ~'
    lensa test '%marks'
    lensd '~&  ~  ~&  %test-marks-end  ~'

    #  measure memory usage post tests
    #
    lensd '~&  ~  ~&  %test-mass-start  ~'
    lensa hood '+hood/mass'
    lensd '~&  ~  ~&  %test-mass-end  ~'

    #  defragment the loom
    #
    lensd '~&  ~  ~&  %pack-start  ~'
    lensa hood '+hood/pack'
    lensd '~&  ~  ~&  %pack-end  ~'

    #  reclaim space within arvo
    #
    lensd '~&  ~  ~&  %trim-start  ~'
    lensa hood '+hood/trim'
    lensd '~&  ~  ~&  %trim-end  ~'

    #  measure memory usage pre |meld
    #
    lensd '~&  ~  ~&  %trim-mass-start  ~'
    lensa hood '+hood/mass'
    lensd '~&  ~  ~&  %trim-mass-end  ~'

    #  globally deduplicate
    #
    lensd '~&  ~  ~&  %meld-start  ~'
    lensa hood '+hood/meld'
    lensd '~&  ~  ~&  %meld-end  ~'

    #  measure memory usage post |meld
    #
    lensd '~&  ~  ~&  %meld-mass-start  ~'
    lensa hood '+hood/mass'
    lensd '~&  ~  ~&  %meld-mass-end  ~'

    lensa hood '+hood/exit'

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

    set +x
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
      if egrep "((FAILED|CRASHED)|warn:) " $f >/dev/null; then
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
}
