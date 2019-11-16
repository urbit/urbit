{ pkgs }:

let

  has = pkgs.haskell.packages.ghc863;
  pat = ../../../pkg/hello-cabal-nix;

in
  has.callCabal2nix "hello" pat {}
