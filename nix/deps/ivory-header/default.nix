{ pkgs, ivory }:

pkgs.stdenv.mkDerivation {
  name              = "ivory.h";
  src               = ivory;
  builder           = ./builder.sh;
  preferLocalBuild  = true;
  nativeBuildInputs = [ pkgs.xxd ];
}
