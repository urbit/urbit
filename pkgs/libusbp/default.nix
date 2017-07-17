{ crossenv }:

let
  version = "4876d89";  # 1.0.2ish

  src = crossenv.nixpkgs.fetchurl {
    url = "https://github.com/pololu/libusbp/archive/${version}.tar.gz";
    sha256 = "1jyvh3h33mxg0qkij5jvsbmagic9wrm9dfhnnilidykmzlqwvp2n";
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
