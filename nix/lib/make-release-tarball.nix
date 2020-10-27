{ lib, stdenvNoCC, coreutils }:

{ name, contents # { target = source, ... }
}:

let

  transforms = builtins.concatStringsSep " " (lib.mapAttrsToList
    (target: source: ''--transform "s,${source},${target},"'') contents);

  sources = builtins.concatStringsSep " "
    (lib.mapAttrsToList (_target: source: "${source}") contents);

in stdenvNoCC.mkDerivation {
  name = "${name}.tar.gz";
  outputs = [ "out" "hash" ];
  nativeBuildInputs = [ coreutils ];
  phases = [ "buildPhase" "hashPhase" ];

  buildPhase = ''
    tar vczf $out \
      --owner=0 --group=0 --mode=u+rw,uga+r \
      --absolute-names \
      --hard-dereference \
      ${transforms} \
      ${sources}
  '';

  hashPhase = ''
    mkdir $hash

    md5sum $out | awk '{printf $1}' > $hash/md5
    sha256sum $out | awk '{printf $1}' > $hash/sha256
  '';

  preferLocalBuild = true;
}
