{ pkgs }:

pkgs.stdenv.mkDerivation rec {
  name    = "secp256k1-b4e87";
  builder = ./builder.sh;

  CFLAGS = "-fPIC";

  configureFlags = [
    "--disable-shared"
    "--enable-module-recovery"
  ];

  buildInputs = [ pkgs.gmp ];
  nativeBuildInputs =
    with pkgs;
    [ autoconf automake libtool m4 ];

  src = pkgs.fetchFromGitHub {
    owner = "bitcoin-core";
    repo = "secp256k1";
    rev = "63150ab4da1ef13ebfb4396064e1ff501dbd015e";
    sha256 = "0s128jgb5r9v92hk95na44kgpwi0dr3bjkqx8k5yxqpwxlhrmzmx";
  };
}
