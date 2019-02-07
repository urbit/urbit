{ crossenv }:

let
  version = "1.19.2";

  name = "xorg-macros-${version}";

  src = crossenv.nixpkgs.fetchurl {
    url = "https://www.x.org/releases/individual/util/util-macros-${version}.tar.gz";
    sha256 = "1dvmkb0c12p94dn280kg9fm2nmpk6ramm9br36bsy3z67mfc89cj";
  };

  lib = crossenv.native.make_derivation {
    inherit version name src;
    builder = ./builder.sh;
    pkgconfig = crossenv.nixpkgs.pkgconfig;
  };

  license = crossenv.native.make_derivation {
    name = "${name}-license";
    inherit src;
    builder = ./license_builder.sh;
  };

  license_set = { "${name}" = license; };

in
  lib // { inherit license_set; }
