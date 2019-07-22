{ crossenv, openssl, zlib }:

crossenv.make_derivation rec {
  name = "curl-${version}";
  version = "7.62.0";

  cross_inputs  = [ crossenv.nixpkgs.perl ];
  native_inputs = [ zlib openssl ];
  builder       = ./builder.sh;

  configureFlags = [
    "--disable-shared"
    "--disable-manual"
    "--disable-ldap"
    "--with-ssl=${openssl}"
  ];

  src = crossenv.nixpkgs.fetchurl {
    url = "https://curl.haxx.se/download/${name}.tar.bz2";
    sha256 = "084niy7cin13ba65p8x38w2xcyc54n3fgzbin40fa2shfr0ca0kq";
  };
}
