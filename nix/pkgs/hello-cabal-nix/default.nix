{ pkgs }:

pkgs.haskellPackages.callCabal2nix "hello" ../../../pkg/hello-cabal-nix {}
