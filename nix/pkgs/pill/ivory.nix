{ lib, stdenvNoCC, fetchGitHubLFS, bootFakeShip, solid, urbit, arvo, curl, xxd
, withGoerli ? false }:

let

  lfs = fetchGitHubLFS { src = ../../../bin/ivory.pill; };

in {
  build = import ./builder.nix {
    inherit stdenvNoCC urbit curl;

    name = "ivory" + lib.optionalString withGoerli "-goerli";
    builder = ./ivory.sh;
    arvo = if withGoerli then arvo.goerli else arvo;
    pier = bootFakeShip {
      inherit urbit;

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
      xxd -i $file > $out/include/ivory_impl.h
    '';

    preferLocalBuild = true;
  };
} // lib.optionalAttrs (!withGoerli) { inherit lfs; }
