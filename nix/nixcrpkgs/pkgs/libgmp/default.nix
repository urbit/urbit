{ crossenv }:

crossenv.make_derivation rec {
  name = "gmp-${version}";
  version = "6.1.2";
  builder = ./builder.sh;
  native_inputs = [ crossenv.nixpkgs.m4 ];

  src = crossenv.nixpkgs.fetchurl {
    urls = [ "mirror://gnu/gmp/${name}.tar.bz2"
             "ftp://ftp.gmplib.org/pub/${name}/${name}.tar.bz2"
           ];
    sha256 = "1clg7pbpk6qwxj5b2mw0pghzawp2qlm3jf9gdd8i6fl6yh2bnxaj";
  };

}
