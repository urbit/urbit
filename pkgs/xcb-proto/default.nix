{ crossenv }:

crossenv.native.make_derivation rec {
  name = "xcb-proto-${version}";
  version = "1.12";

  src = crossenv.nixpkgs.fetchurl {
    url = "https://xcb.freedesktop.org/dist/xcb-proto-${version}.tar.bz2";
    sha256 = "01j91946q8f34l1mbvmmgvyc393sm28ym4lxlacpiav4qsjan8jr";
  };

  builder = ./builder.sh;

  native_inputs = [ crossenv.nixpkgs.python2 ];
}
