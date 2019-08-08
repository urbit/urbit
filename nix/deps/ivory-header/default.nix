{
  pkgs,
  ivory ? ../../../bin/ivory.pill
}:

pkgs.stdenv.mkDerivation {
  name              = "ivory.h";
  builder           = ./builder.sh;
  nativeBuildInputs = with pkgs; [ xxd ];
  IVORY             = ivory;
}
