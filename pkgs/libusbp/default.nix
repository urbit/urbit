{ crossenv }:

let
  version = "1.0.2";

  src = crossenv.nixpkgs.fetchurl {
    url = "https://github.com/pololu/libusbp/archive/${version}.tar.gz";
    sha256 = "04r2b5v226j4mvc60m0hsl2w20x4c5h0qh0619nn21kkkv15sirb";
  };

  lib = crossenv.make_derivation {
    name = "libusbp-${version}";
    inherit src;
    builder = ./builder.sh;
  };

  license_fragment = crossenv.make_native_derivation {
    name = "libusbp-${version}-license-fragment";
    inherit src;
    builder = ./license_builder.sh;
  };
in
  lib // { inherit license_fragment; }
