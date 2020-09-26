{ lib, stdenvNoCC, python }:

# We don't want to use the following - as withPackages actually creates
# a wrapper script to correctly set PYTHONPATH. This script is then set
# as the shebang for herb - which is not allowed on Darwin.
# python.withPackages (py: [ py.requests ])) ];
#
# Instead, calling wrapPythonPrograms allows cross platform usage.

stdenvNoCC.mkDerivation {
  name  = "herb";
  src = ./herb;

  nativeBuildInputs = [ python.pkgs.wrapPython ];
  buildInputs = [ python python.pkgs.requests ];
  pythonPath = [ python.pkgs.requests ];

  phases = [ "installPhase" "fixupPhase" ];

  installPhase = ''
    mkdir -p $out/bin 
    cp $src $out/bin/herb
    chmod +x $out/bin/herb
  '';

  postFixup = ''
    wrapPythonPrograms
  '';
}
