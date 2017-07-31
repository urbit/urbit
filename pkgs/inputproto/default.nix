{ crossenv }:

crossenv.native.make_derivation rec {
  name = "inputproto-${version}";
  version = "2.3.2";

  src = crossenv.nixpkgs.fetchurl {
    url = "https://xorg.freedesktop.org/releases/individual/proto/inputproto-${version}.tar.bz2";
    sha256 = "07gk7v006zqn3dcfh16l06gnccy7xnqywf3vl9c209ikazsnlfl9";
  };

  builder = ./builder.sh;
}
