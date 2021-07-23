{ lib, stdenvNoCC, bc }:

stdenvNoCC.mkDerivation {
  name = "arvo";
  src = lib.cleanSource ../../../pkg/arvo;

  buildInputs = [ bc ];

  outputs = [ "out" "ropsten" ];

  phases = [ "mainnetPhase" "ropstenPhase" ];

  mainnetPhase = ''
    cp -r $src/ $out
    chmod -R u+w $out
  '';

  ropstenPhase = ''
    cp -r $src tmp
    chmod -R u+w tmp

    ZUSE=tmp/sys/zuse.hoon
    AMES=tmp/sys/vane/ames.hoon
    ACME=tmp/app/acme.hoon

    # Replace the mainnet azimuth contract with the ropsten contract
    sed --in-place \
      's/\(\+\+  contracts  \)mainnet\-contracts/\1ropsten-contracts/' \
      $ZUSE

    # Increment the %ames protocol version
    sed -r --in-place \
      's_^(=/  protocol\-version=\?\(.*\)  %)([0-7])_echo "\1$(echo "(\2+1) % 8" | bc)"_e' \
      $AMES

    # Use the staging API in :acme
    sed --in-place \
      's_https://acme-v02.api.letsencrypt.org/directory_https://acme-staging-v02.api.letsencrypt.org/directory_' \
      $ACME

    cp -r tmp $ropsten
    chmod -R u+w $ropsten
  '';

  preferLocalBuild = true;
}
