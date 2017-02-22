# TODO: break in the "initialization of EGLWindow" and see where it fails

{ crossenv, angle, angle_util }:

crossenv.nixpkgs.stdenv.mkDerivation rec {
  name = "angle_samples-${angle.version}-${crossenv.host}";

  src = angle.src;

  buildInputs = [
    crossenv.gcc
    crossenv.binutils
  ];

  inherit angle angle_util;

  host = crossenv.host;

  builder = ./samples_builder.sh;
}
