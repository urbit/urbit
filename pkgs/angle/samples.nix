{ crossenv, angle }:

crossenv.nixpkgs.stdenv.mkDerivation rec {
  name = "${angle.name}-samples";

  src = angle.src;

  buildInputs = [
    crossenv.gcc
    crossenv.binutils
  ];

  inherit angle;

  host = crossenv.host;

  builder = ./samples_builder.sh;
}
