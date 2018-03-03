{ crossenv }:

crossenv.make_derivation rec {
  name = "expat-${version}";

  version = "2.2.0";

  src = crossenv.nixpkgs.fetchurl {
    # Sourceforge went down.  The original URL we used was:
    # mirror://sourceforge/expat/expat-${version}.tar.bz2
    url = "https://files.tmphax.com/repo1/expat-${version}.tar.bz2";
    sha256 = "1zq4lnwjlw8s9mmachwfvfjf2x3lk24jm41746ykhdcvs7r0zrfr";
  };

  patches = [
    ./cve-2016-0718.patch
  ];

  builder = ./builder.sh;
}
