{ crossenv }:

let
  version = "1.3.5";

  name = "xtrans-${version}";

  src = crossenv.nixpkgs.fetchurl {
    url = "https://xorg.freedesktop.org/releases/individual/lib/xtrans-${version}.tar.bz2";
    sha256 = "00c3ph17acnsch3gbdmx33b9ifjnl5w7vx8hrmic1r1cjcv3pgdd";
  };

  lib = crossenv.native.make_derivation rec {
    inherit version name src;
    builder = ./builder.sh;
  };

  license = crossenv.native.make_derivation {
    name = "${name}-license";
    inherit src;
    builder = ./license_builder.sh;
  };

  license_set = { "${name}" = license; };

in
  lib // { inherit license_set; }
