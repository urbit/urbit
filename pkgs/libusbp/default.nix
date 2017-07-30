{ crossenv, libudev }:

let
  version = "4752d45";  # 1.0.2ish

  src = crossenv.nixpkgs.fetchurl {
    url = "https://github.com/pololu/libusbp/archive/${version}.tar.gz";
    sha256 = "1agwa0zml2vzi10mmpsymwbd20llkwkcz656q65595pi24v710si";
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
