{ pkgs ? import ../../nixpkgs.nix }:

let

  compiler    = "default";
  doBenchmark = false;

  run-hpack =
    "${pkgs.haskellPackages.hpack}/bin/hpack";

  f = { mkDerivation, stdenv,
        base, classy-prelude, lens, hpack, megaparsec }:
      mkDerivation {
        pname = "uterm";
        version = "0.1.0.0";
        src = ../../../pkg/uterm;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          base classy-prelude lens hpack megaparsec
        ];
        license = stdenv.lib.licenses.lgpl3;
        preConfigure = ''
          ${run-hpack}
        '';
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;
  drv     = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
