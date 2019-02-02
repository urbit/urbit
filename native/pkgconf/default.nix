{ env }:

env.make_derivation rec {
  name = "pkgconf-${version}";

  version = "1.6.0";

  src = env.nixpkgs.fetchurl {
    url = "https://github.com/pkgconf/pkgconf/archive/pkgconf-${version}.tar.gz";
    sha256 = "1j3700iyjvd4m4ahf827lzbzlji6q3higrnynqhdk2zklxq8shml";
  };

  native_inputs = [
    env.nixpkgs.autoconf
    env.nixpkgs.automake
    env.nixpkgs.libtool
    env.nixpkgs.m4
  ];

  ACLOCAL_PATH = "${env.nixpkgs.libtool}/share/aclocal";

  builder = ./builder.sh;
}
