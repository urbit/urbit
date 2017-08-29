{ crossenv, libudev }:

let
  version = "1.0.4";

  src = crossenv.nixpkgs.fetchurl {
    url = "https://github.com/pololu/libusbp/archive/${version}.tar.gz";
    sha256 = "154m65wfzvw62w7lif6mpsn5bh7fj2hk8zw6z7q6s44ffjq30rdg";
  };

  lib = crossenv.make_derivation {
    name = "libusbp-${version}";
    inherit src version;
    builder = ./builder.sh;

    cross_inputs =
      if crossenv.os == "linux" then
        [ libudev ]
      else
        [];

    libudev = if crossenv.os == "linux" then libudev else null;
  };

  examples = crossenv.make_derivation {
    name = "libusbp-examples-${version}";
    inherit src version;
    builder = ./examples_builder.sh;
    cross_inputs = [ lib ];
  };

  license_fragment = crossenv.native.make_derivation {
    name = "libusbp-${version}-license-fragment";
    inherit src;
    builder = ./license_builder.sh;
    input_license_fragments =
      if crossenv.os == "linux" then
        [ libudev.license_fragment ]
      else
        [];
  };
in
  lib // { inherit examples license_fragment; }
