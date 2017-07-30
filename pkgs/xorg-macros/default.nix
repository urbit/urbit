{ crossenv }:

crossenv.native.make_derivation rec {
  name = "xorg-macros-${version}";
  version = "1.19.1";

  src = crossenv.nixpkgs.fetchurl {
    url = "https://www.x.org/releases/individual/util/util-macros-1.19.1.tar.gz";
    sha256 = "1f27cmbxq0kdyvqsplxpsi9pxm5qy45lcagxr9gby2hy3pjd0aj7";
  };

  builder = ./builder.sh;
}
