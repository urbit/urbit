{ crossenv, angle, angle_util }:

crossenv.make_derivation rec {
  name = "angle-samples-${angle.version}";

  src = angle.src;

  inherit angle angle_util;

  builder = ./samples_builder.sh;
}
