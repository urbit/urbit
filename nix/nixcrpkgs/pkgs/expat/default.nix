{ crossenv }:

crossenv.make_derivation rec {
  name = "expat-${version}";

  version = "2.2.6";

  src = crossenv.nixpkgs.fetchurl {
    # The original URL was:
    # mirror://sourceforge/expat/expat-${version}.tar.bz2
    url = "https://files.tmphax.com/repo1/expat-${version}.tar.bz2";
    sha256 = "1wl1x93b5w457ddsdgj0lh7yjq4q6l7wfbgwhagkc8fm2qkkrd0p";
  };

  patches = [];

  builder = ./builder.sh;
}
