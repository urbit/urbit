{ crossenv }:

crossenv.native.make_derivation rec {
  name = "xextproto-${version}";
  version = "7.3.0";

  src = crossenv.nixpkgs.fetchurl {
    url = "https://xorg.freedesktop.org/releases/individual/proto/xextproto-${version}.tar.bz2";
    sha256 = "1c2vma9gqgc2v06rfxdiqgwhxmzk2cbmknwf1ng3m76vr0xb5x7k";
  };

  builder = ./builder.sh;
}
