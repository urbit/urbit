{ pkgs, uv }:

pkgs.stdenv.mkDerivation {
  name        = "h2o";
  src         = pkgs.sources.h2o;
  builder     = ./builder.sh;
  buildInputs = [ uv pkgs.openssl pkgs.zlib ];
}
