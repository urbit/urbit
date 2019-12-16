{ pkgs }:

pkgs.stdenv.mkDerivation {
  name = "arvo";
  builder = ./builder.sh;
  src = pkgs.buildRustCrateHelpers.exclude [ ".git" ] ../../../pkg/arvo;
  meta = {
    priority = 0;
  };
}
