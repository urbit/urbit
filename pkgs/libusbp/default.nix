{ crossenv, libudev }:

let
  version = "1.0.4";

  name = "libusbp-${version}";

  src = crossenv.nixpkgs.fetchurl {
    url = "https://github.com/pololu/libusbp/archive/${version}.tar.gz";
    sha256 = "154m65wfzvw62w7lif6mpsn5bh7fj2hk8zw6z7q6s44ffjq30rdg";
  };

  lib = crossenv.make_derivation {
    inherit version name src;
    builder = ./builder.sh;

    cross_inputs =
      if crossenv.os == "linux" then
        [ libudev ]
      else
        [];

    libudev = if crossenv.os == "linux" then libudev else null;
  };

  examples = crossenv.make_derivation {
    name = "${name}-examples";
    inherit src version;
    builder = ./examples_builder.sh;
    cross_inputs = [ lib ];
  };

  license = crossenv.native.make_derivation {
    name = "${name}-license";
    inherit src;
    builder = ./license_builder.sh;
  };

  license_set =
    (if crossenv.os == "linux" then libudev.license_set else {}) //
    { libusbp = license; };
in
  lib // { inherit examples license_set; }
