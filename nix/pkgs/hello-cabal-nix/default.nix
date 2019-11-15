{ pkgs }:

pkgs.pkgs.haskell.packages.ghc863.callPackage ./hello.nix { }
