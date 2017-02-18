{ crossenv }:

crossenv.nixpkgs.stdenv.mkDerivation rec {
  name = "expat-${version}-${crossenv.host}";

  version = "2.2.0";

  src = crossenv.nixpkgs.fetchurl {
    url = "https://downloads.sourceforge.net/expat/expat-${version}.tar.bz2";
    sha256 = "1zq4lnwjlw8s9mmachwfvfjf2x3lk24jm41746ykhdcvs7r0zrfr";
  };

  patches = [
    ./cve-2016-0718.patch
  ];

  builder = ./builder.sh;

  buildInputs = [
    crossenv.gcc
    crossenv.binutils
  ];

  inherit (crossenv) host;
}
