{ stdenvNoCC, urbit, arvo, curl, name, builder, pier }:

stdenvNoCC.mkDerivation {
  name = "${name}.pill";
  src = pier;
  buildInputs = [ curl urbit ];
  dontUnpack = true;

  buildPhase = builtins.readFile builder;

  installPhase = ''
    mv ${name}.pill $out
  '';

  ARVO = arvo;
}
