{ crossenv, angle, angle_util }:

crossenv.nixpkgs.stdenv.mkDerivation rec {
  name = "${angle.name}-samples";

  src = angle.src;

  buildInputs = [
    crossenv.gcc
    crossenv.binutils
  ];

  inherit angle angle_util;

  host = crossenv.host;

  builder = ./samples_builder.sh;
}
