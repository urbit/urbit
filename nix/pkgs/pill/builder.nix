{ stdenvNoCC, urbit, arvo, herb, name, builder, pier }:

stdenvNoCC.mkDerivation {
  name = "${name}.pill";
  src = pier;
  outputs = [ "out" "hash" ];
  buildInputs = [ urbit herb ];

  dontUnpack = true;

  buildPhase = builtins.readFile builder;

  installPhase = ''
    mv ${name}.pill $out

    mkdir $hash

    md5sum $out | awk '{printf $1}' > $hash/md5
    sha256sum $out | awk '{printf $1}' > $hash/sha256
  '';

  ARVO = arvo;
}
