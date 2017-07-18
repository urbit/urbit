{ crossenv, libudev }:

let
  version = "d440c9a";  # 1.0.2ish

  src = crossenv.nixpkgs.fetchurl {
    url = "https://github.com/pololu/libusbp/archive/${version}.tar.gz";
    sha256 = "0wq9sgqr55qvn6rycl4khyzwjbk2y79nfs5p74x5q39y9hq9mvj4";
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

  license_fragment = crossenv.make_native_derivation {
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
  lib // { inherit license_fragment; }
