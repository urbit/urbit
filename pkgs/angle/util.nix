{ crossenv, angle }:

crossenv.nixpkgs.stdenv.mkDerivation rec {
  name = "${angle.name}-util";

  src = angle.src;

  buildInputs = [
    crossenv.gcc
    crossenv.binutils
  ];

  inherit angle;

  host = crossenv.host;
  os = crossenv.os;

  builder = ./util_builder.sh;

  meta = {
    description = "libangle_util is a helper library for programs like tests " +
      "and samples that surround ANGLE but that are not the ANGLE libraries " +
      "themselves";
  };
}
