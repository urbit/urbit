{ crossenv }:

let
  version = "1.12";

  name = "xcb-proto-${version}";

  src = crossenv.nixpkgs.fetchurl {
    url = "https://xcb.freedesktop.org/dist/xcb-proto-${version}.tar.bz2";
    sha256 = "01j91946q8f34l1mbvmmgvyc393sm28ym4lxlacpiav4qsjan8jr";
  };

  lib = crossenv.native.make_derivation rec {
    inherit version name src;
    builder = ./builder.sh;
    native_inputs = [ crossenv.nixpkgs.python2 ];
  };

  license = crossenv.native.make_derivation {
    name = "${name}-license";
    inherit src;
    builder = ./license_builder.sh;
  };

  license_set = { "${name}" = license; };

in
  lib // { inherit license_set; }
