let

  rev  = "61c3169a0e17d789c566d5b241bfe309ce4a6275";
  hash = "0qbycg7wkb71v20rchlkafrjfpbk2fnlvvbh3ai9pyfisci5wxvq";

  nixpkgs = builtins.fetchTarball {
    name = "nixpkgs-2019-01-15";
    url = "https://github.com/nixos/nixpkgs/archive/${rev}.tar.gz";
    sha256 = hash;
  };

in

{ pkgs ? import nixpkgs {} }:

let

  pyenv = pkgs.python2.withPackages (py: [ py.requests ]);
  pyexe = "${pyenv}/bin/python";

in

pkgs.stdenv.mkDerivation rec {
  name         = "herb";
  buildInputs  = [ pyenv ];
  unpackPhase  = "true";
  installPhase = ''
    mkdir -p $out/bin

    cp ${./herb} $out/bin/herb.py

    cat > $out/bin/herb <<EOF
    #!/usr/bin/env bash
    ${pyexe} $out/bin/herb.py "\$@"
    EOF

    chmod +x $out/bin/herb
  '';
}
