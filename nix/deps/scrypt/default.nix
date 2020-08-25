{ pkgs }:

pkgs.stdenv.mkDerivation {
  name    = "scrypt";
  src     = pkgs.sources.libscrypt;
  builder = ./builder.sh;
}
