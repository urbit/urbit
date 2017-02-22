{ crossenv, gdb, angle, examples }:

crossenv.nixpkgs.stdenv.mkDerivation rec {
  name = "angle_debug_bundle-${angle.version}-${crossenv.host}";

  builder = ./debug_bundle_builder.sh;

  inherit (angle) src;

  inherit gdb angle examples;
}
