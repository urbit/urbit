{ crossenv }:

let
  version = "1.19.1";

  name = "xorg-macros-${version}";

  src = crossenv.nixpkgs.fetchurl {
    url = "https://www.x.org/releases/individual/util/util-macros-1.19.1.tar.gz";
    sha256 = "1f27cmbxq0kdyvqsplxpsi9pxm5qy45lcagxr9gby2hy3pjd0aj7";
  };

  lib = crossenv.native.make_derivation {
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
