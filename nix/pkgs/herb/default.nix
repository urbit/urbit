{ lib, stdenvNoCC, python }:

# Avoid using `python.withPackages` as it creates a wrapper script to set
# PYTHONPATH, and the script is used verbatim as a python shebang.
#
# Unfortunately Darwin does not allow scripts as a shebang - so to get a
# cross platform python interpreter with appropriate site-packages setup
# we use `wrapPython/Packages` which handles these cases correctly.

stdenvNoCC.mkDerivation {
  name = "herb";
  src = ../../../pkg/herb/herb;

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
