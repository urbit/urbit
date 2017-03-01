{ crossenv, pdcurses }:

crossenv.nixpkgs.stdenv.mkDerivation rec {
  name = "pdcurses_demos-${version}-${crossenv.host}";

  inherit pdcurses;
  inherit (pdcurses) src version;

  builder = ./demos_builder.sh;

  buildInputs = [
    crossenv.gcc
    crossenv.binutils
  ];

  inherit (crossenv) host;
}

