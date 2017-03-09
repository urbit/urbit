{ crossenv, angle, angle_util }:

crossenv.nixpkgs.stdenv.mkDerivation rec {
  name = "angle_samples-${angle.version}-${crossenv.host}";

  src = angle.src;

  buildInputs = [
    crossenv.gcc
    crossenv.binutils
  ];

  inherit angle angle_util;

  inherit (crossenv) host exe_suffix;

  builder = ./samples_builder.sh;
}
