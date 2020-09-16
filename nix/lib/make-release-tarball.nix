{ lib, stdenvNoCC, coreutils }:

{ name
, contents # { target = source, ... }
}:

let

  transforms =
    builtins.concatStringsSep " "
      (lib.mapAttrsToList
        (target: source: ''--transform "s,${source},${target},"'')
        contents);

  sources =
    builtins.concatStringsSep " "
      (lib.mapAttrsToList
        (_target: source: "${source}")
        contents);

in stdenvNoCC.mkDerivation {
  name = "${name}.tar.gz";
  outputs = [ "out" "md5" ];
  nativeBuildInputs = [ coreutils ];
  phases = [ "installPhase" ];

  installPhase = ''
    tar vczf $out \
      --owner=0 --group=0 --mode=u+rw,uga+r \
      --absolute-names \
      --hard-dereference \
      ${transforms} \
      ${sources}
    
    md5sum $out | awk '{printf $1}' > $md5
  '';

  preferLocalBuild = true;
}
