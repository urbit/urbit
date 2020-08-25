{ pkgs }:

pkgs.stdenv.mkDerivation {
  name    = "ed25519";
  src     = pkgs.sources.ed25519;
  builder = ./builder.sh;
}
