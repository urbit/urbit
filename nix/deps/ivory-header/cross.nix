{ crossenv, ivory }:

crossenv.make_derivation {
  name             = "ivory.h";
  src              = ivory;
  builder          = ./builder.sh;
  preferLocalBuild = true;
  native_inputs    = [ crossenv.nixpkgs.xxd ];
}
