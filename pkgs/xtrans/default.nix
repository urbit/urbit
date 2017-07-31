{ crossenv }:

crossenv.native.make_derivation rec {
  name = "xtrans-${version}";
  version = "1.3.5";

  src = crossenv.nixpkgs.fetchurl {
    url = "https://xorg.freedesktop.org/releases/individual/lib/xtrans-${version}.tar.bz2";
    sha256 = "00c3ph17acnsch3gbdmx33b9ifjnl5w7vx8hrmic1r1cjcv3pgdd";
  };

  builder = ./builder.sh;
}
