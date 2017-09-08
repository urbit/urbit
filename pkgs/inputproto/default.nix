{ crossenv }:

let
  version = "2.3.2";

  name = "inputproto-${version}";

  src = crossenv.nixpkgs.fetchurl {
    url = "https://xorg.freedesktop.org/releases/individual/proto/inputproto-${version}.tar.bz2";
    sha256 = "07gk7v006zqn3dcfh16l06gnccy7xnqywf3vl9c209ikazsnlfl9";
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
