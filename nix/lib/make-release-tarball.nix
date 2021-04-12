{ lib, stdenvNoCC, coreutils }:

{ name, extension ? "tgz", contents # { target = source, ... }
}:

let

  transforms = builtins.concatStringsSep " " (lib.mapAttrsToList
    (target: source: ''--transform "s,${source},${target},"'') contents);

  sources = builtins.concatStringsSep " "
    (lib.mapAttrsToList (_target: source: "${source}") contents);

in stdenvNoCC.mkDerivation {
  name = "${name}.${extension}";
  phases = [ "buildPhase" ];

  nativeBuildInputs = [ coreutils ];

  buildPhase = ''
    tar -vczf $out \
      --owner=0 --group=0 --mode=u+rw,uga+r \
      --absolute-names \
      --hard-dereference \
      ${transforms} \
      ${sources}
  '';

  preferLocalBuild = true;
}
