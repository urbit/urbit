{ lib, stdenvNoCC, fetchGitHubLFS, bootFakeShip, solid, urbit, arvo, herb, xxd
, withRopsten ? false }:

let

  lfs = fetchGitHubLFS { src = ../../../bin/ivory.pill; };

in {
  build = import ./builder.nix {
    inherit stdenvNoCC urbit herb;

    name = "ivory" + lib.optionalString withRopsten "-ropsten";
    builder = ./ivory.sh;
    arvo = if withRopsten then arvo.ropsten else arvo;
    pier = bootFakeShip {
      inherit urbit herb;

      pill = solid.lfs;
      ship = "zod";
    };
  };

  # The hexdump of the `.lfs` pill contents as a C header.
  header = stdenvNoCC.mkDerivation {
    name = "ivory-header";
    src = lfs;
    nativeBuildInputs = [ xxd ];
    phases = [ "installPhase" ];

    installPhase = ''
      file=u3_Ivory.pill

      header "writing $file"

      mkdir -p $out/include
      cat $src > $file
      xxd -i $file > $out/include/ivory.h
    '';

    preferLocalBuild = true;
  };
} // lib.optionalAttrs (!withRopsten) { inherit lfs; }
