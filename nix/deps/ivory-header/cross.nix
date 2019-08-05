{
  crossenv,
  ivory ? ../../../bin/ivory.pill
}:

crossenv.make_derivation rec {
  name              = "ivory.h";
  builder           = ./builder.sh;
  native_inputs     = with crossenv.nixpkgs; [ xxd ];
  IVORY             = ivory;
}
