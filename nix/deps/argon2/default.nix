{ pkgs }:

pkgs.stdenv.mkDerivation {
  name    = "argon2";
  src     = pkgs.sources.argon2;
  builder = ./builder.sh;

  NO_THREADS = true;
}
