{ lib, stdenvNoCC, bc }:

stdenvNoCC.mkDerivation {
  name = "sources";
  src = lib.cleanSource ../../../pkg;

  buildInputs = [ bc ];

  outputs = [ "out" "goerli" ];

  phases = [ "mainnetPhase" "goerliPhase" ];

  mainnetPhase = ''
    cp -r $src $out
    chmod -R u+w $out
  '';

  goerliPhase = ''
    cp -r $src tmp
    chmod -R u+w tmp

    ZUSE=tmp/arvo/sys/zuse.hoon
    AMES=tmp/arvo/sys/vane/ames.hoon
    ACME=tmp/arvo/app/acme.hoon

    # Replace the mainnet azimuth contract with the goerli contract
    sed --in-place \
      's/\(\+\+  contracts  \)mainnet\-contracts/\1goerli-contracts/' \
      $ZUSE

    # Increment the %ames protocol version
    sed -r --in-place \
      's_^(=/  protocol\-version=\?\(.*\)  %)([0-7])_echo "\1$(echo "(\2+1) % 8" | bc)"_e' \
      $AMES

    # Use the staging API in :acme
    sed --in-place \
      's_https://acme-v02.api.letsencrypt.org/directory_https://acme-staging-v02.api.letsencrypt.org/directory_' \
      $ACME

    cp -r tmp $goerli
    chmod -R u+w $goerli
  '';

  preferLocalBuild = true;
}
