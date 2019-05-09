{ crossenv }:

crossenv.make_derivation rec {
  name = "libsigsegv-${version}";
  version = "2.12";

  src = crossenv.nixpkgs.fetchurl {
    url = "mirror://gnu/libsigsegv/${name}.tar.gz";
    sha256 = "1dlhqf4igzpqayms25lkhycjq1ccavisx8cnb3y4zapbkqsszq9s";
  };

  builder = ./builder.sh;
}
