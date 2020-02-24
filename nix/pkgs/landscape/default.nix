{ pkgs }:

pkgs.stdenv.mkDerivation rec {
  name = "landscape";
  builder = ./builder.sh;
  src = builtins.filterSource
    (path: type: baseNameOf path != "node_modules")
    ../../../pkg/landscape;
}
