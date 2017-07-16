{ crossenv }:

let
  version = "70a33dc";  # 1.0.2ish

  src = crossenv.nixpkgs.fetchurl {
    url = "https://github.com/pololu/libusbp/archive/${version}.tar.gz";
    sha256 = "1y0rln6cp6m3zd5k7lynrwk00w3jvqlv9qyx2cjhl6j90330n53d";
  };

  lib = crossenv.make_derivation {
    name = "libusbp-${version}";
    inherit src version;
    builder = ./builder.sh;
  };

  license_fragment = crossenv.make_native_derivation {
    name = "libusbp-${version}-license-fragment";
    inherit src;
    builder = ./license_builder.sh;
  };
in
  lib // { inherit license_fragment; }
