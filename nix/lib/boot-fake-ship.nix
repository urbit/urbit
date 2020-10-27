{ stdenvNoCC, cacert }:

{ urbit, herb, arvo ? null, pill, ship }:

stdenvNoCC.mkDerivation {
  name = "fake-${ship}";

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
      urbit -d -F "$SHIP" -B "$PILL" ./pier
    else
      urbit -d -F "$SHIP" -A "$ARVO" -B "$PILL" ./pier
    fi

    cleanup () {
      if [ -f ./pier/.vere.lock ]; then
        kill $(< ./pier/.vere.lock) || true
      fi

      set +x
    }

    trap cleanup EXIT

    check () {
      [ 3 -eq "$(herb ./pier -d 3)" ]
    }

    if check && sleep 10 && check; then
      header "boot success"
      herb ./pier -p hood -d '+hood/exit'
    else
      header "boot failure"
      kill $(< ./pier/.vere.lock) || true
      exit 1
    fi
  '';

  installPhase = ''
    mv ./pier $out
  '';
}
