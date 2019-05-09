{ pkgs }:

pkgs.stdenv.mkDerivation rec {
  name = "argon2-4da94";
  builder = ./builder.sh;
  NO_THREADS = true;
  src = pkgs.fetchFromGitHub {
    owner = "urbit";
    repo = "argon2";
    rev = "4da94a611ee62bad87ab2b131ffda3bcc0723d9c";
    sha256 = "0bqq1hg367l4jkb6cqhxlblpvdbwz3l586qsfakwzfd9wdvnm3yc";
  };
}
