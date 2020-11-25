# Functions that are expected run on the native (non-cross) system.

{ lib, recurseIntoAttrs, haskell-nix, callPackage }:

rec {
  bootFakeShip = callPackage ./boot-fake-ship.nix { };

  testFakeShip = callPackage ./test-fake-ship.nix { inherit bootFakeShip; };

  fetchGitHubLFS = callPackage ./fetch-github-lfs.nix { };

  makeReleaseTarball = callPackage ./make-release-tarball.nix { };

  collectHaskellComponents = project:
    let

      # These functions pull out from the Haskell project either all the
      # components of a particular type, or all the checks.

      pkgs = haskell-nix.haskellLib.selectProjectPackages project;

      collectChecks = _:
        recurseIntoAttrs (builtins.mapAttrs (_: p: p.checks) pkgs);

      collectComponents = type:
        haskell-nix.haskellLib.collectComponents' type pkgs;

      # Recompute the Haskell package set sliced by component type
    in builtins.mapAttrs (type: f: f type) {
      # These names must match the subcomponent: components.<name>.<...>
      "library" = collectComponents;
      "tests" = collectComponents;
      "benchmarks" = collectComponents;
      "exes" = collectComponents;
      "checks" = collectChecks;
    };
}
