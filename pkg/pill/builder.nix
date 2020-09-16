{ stdenvNoCC, urbit, arvo, herb, name, builder, pier }:

stdenvNoCC.mkDerivation {
  inherit builder;

  name = "${name}.pill";
  src = pier;
  outputs = [ "out" "md5" ];
  buildInputs = [ urbit herb ];

  ARVO = arvo;
}
