{ pkgs }:

pkgs.stdenv.mkDerivation {
  name = "arvo-ropsten";
  buildInputs = [ pkgs.bc ];
  builder = ./builder.sh;
  src = pkgs.buildRustCrateHelpers.exclude [ ".git" ] ../../../pkg/arvo;
}
