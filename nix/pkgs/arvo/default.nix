{ lib, stdenvNoCC, marsSources }:

stdenvNoCC.mkDerivation {
  name = "arvo";

  src = marsSources;

  outputs = [ "out" "ropsten" ];

  phases = [ "mainnetPhase" "ropstenPhase" ];

  mainnetPhase = ''
    ln -s ${marsSources.out}/arvo $out
    '';

  ropstenPhase = ''
    ln -s ${marsSources.ropsten}/arvo $ropsten
    '';

  preferLocalBuild = true;
}
