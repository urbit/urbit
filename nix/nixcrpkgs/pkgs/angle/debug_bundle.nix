{ crossenv, gdb, angle, examples }:

crossenv.make_derivation rec {
  name = "angle_debug_bundle-${angle.version}";

  builder = ./debug_bundle_builder.sh;

  inherit (angle) src;

  inherit gdb angle examples;
}
