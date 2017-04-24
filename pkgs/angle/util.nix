# libangle_util is a helper library for programs like tests
# and samples that surround ANGLE but that are not the ANGLE libraries
# themselves

{ crossenv, angle }:

crossenv.make_derivation rec {
  name = "angle_util-${angle.version}";

  src = angle.src;

  inherit angle;

  builder = ./util_builder.sh;
}
