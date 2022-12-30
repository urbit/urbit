{ lib, stdenvNoCC, curl }:

{ urbit, arvo ? null, pill, ship, arguments ? [ "-l" ] }:

let

  args = arguments ++ [ "-d" "-F" "${ship}" "-B" "${pill}" ]
    ++ lib.optionals (arvo != null) [ "-A" "${arvo}" ];

in stdenvNoCC.mkDerivation {
  name = "fake-${ship}";

  buildInputs = [ curl urbit ];

  phases = [ "buildPhase" "installPhase " ];

  buildPhase = ''
    set -xeuo pipefail

    urbit ${lib.concatStringsSep " " args} ./pier

    cleanup () {
      if [ -f ./pier/.vere.lock ]; then
        kill $(< ./pier/.vere.lock) || true
      fi

      set +x
    }

    trap cleanup EXIT

    port=$(cat ./pier/.http.ports | grep loopback | tr -s ' ' '\n' | head -n 1)

    lensd() {
      curl -f -s                                                           \
        --data "{\"source\":{\"dojo\":\"$1\"},\"sink\":{\"stdout\":null}}" \
        "http://localhost:$port" | xargs printf %s | sed 's/\\n/\n/g'
    }

    lensa() {
      curl -f -s                                                           \
        --data "{\"source\":{\"dojo\":\"$2\"},\"sink\":{\"app\":\"$1\"}}"  \
        "http://localhost:$port" | xargs printf %s | sed 's/\\n/\n/g'
    }

    check () {
      [ 3 -eq "$(lensd 3)" ]
    }

    if check && sleep 10 && check; then
      header "boot success"
      lensa hood '+hood/exit'
      while [ -f ./pier/.vere.lock ]; do
        echo "waiting for pier to shut down"
        sleep 5
      done
    else
      header "boot failure"
      kill $(< ./pier/.vere.lock) || true
      set +x
      exit 1
    fi

    set +x
  '';

  installPhase = ''
    ls
    ls -a ./pier
    mv ./pier $out
  '';
}
