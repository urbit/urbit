{ stdenvNoCC, urbit, arvo, herb, name, builder, pier }:

stdenvNoCC.mkDerivation {
  name = "${name}.pill";
  src = pier;
  buildInputs = [ urbit herb ];
  dontUnpack = true;

  buildPhase = builtins.readFile builder;

  installPhase = ''
    mv ${name}.pill $out
  '';

  ARVO = arvo;
}
