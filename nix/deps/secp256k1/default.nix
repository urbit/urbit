{ pkgs }:

pkgs.stdenv.mkDerivation {
  name    = "secp256k1";
  src     = pkgs.sources.secp256k1;
  builder = ./builder.sh;

  nativeBuildInputs = [ pkgs.autoconf pkgs.automake pkgs.libtool pkgs.m4 ];
  buildInputs       = [ pkgs.gmp ];

  configureFlags = ["--disable-shared" "--enable-module-recovery" ];
  CFLAGS         = "-fPIC";
}
