{ lib, stdenvNoCC, marsSources }:

stdenvNoCC.mkDerivation {
  name = "arvo";

  src = marsSources;

  outputs = [ "out" "goerli" ];

  phases = [ "mainnetPhase" "goerliPhase" ];

  mainnetPhase = ''
    ln -s ${marsSources.out}/arvo $out
    '';

  goerliPhase = ''
    ln -s ${marsSources.goerli}/arvo $goerli
    '';

  preferLocalBuild = true;
}
