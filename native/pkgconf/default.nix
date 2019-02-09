# We use pkgconf instead of pkg-config because pkg-config considers it to be
# an error when there are two "Libs" fields in a .pc file.  Because we do
# static linking, we often need to edit the pc files with sed to change
# Libs.private into a second Libs field.

{ env }:

env.make_derivation rec {
  name = "pkgconf-${version}";

  version = "1.6.0";

  src = env.nixpkgs.fetchurl {
    url = "https://github.com/pkgconf/pkgconf/archive/pkgconf-${version}.tar.gz";
    sha256 = "1j3700iyjvd4m4ahf827lzbzlji6q3higrnynqhdk2zklxq8shml";
  };

  patches = [
    # Fix a bug in pkgconf that causes it to ignore entries on its path
    # that are symbolic links.
    ./do-not-read-link.patch

    # Fix a bug in pkgconf that causes it to silently skip .pc files
    # missing the "Description" field.
    ./do-not-require-description.patch
  ];

  native_inputs = [
    env.nixpkgs.autoconf
    env.nixpkgs.automake
    env.nixpkgs.libtool
    env.nixpkgs.m4
  ];

  ACLOCAL_PATH = "${env.nixpkgs.libtool}/share/aclocal";

  builder = ./builder.sh;
}
