{ lib, recurseIntoAttrs, haskell-nix, callPackage }:

let

  # Fetchers + Import from derivations (IFDs)
  fetchers = rec {
    bootFakeShip = callPackage ./boot-fake-ship.nix { };

    testFakeShip = callPackage ./test-fake-ship.nix { inherit bootFakeShip; };

    fetchGitHubLFS = callPackage ./fetch-github-lfs.nix { };

    makeReleaseTarball = callPackage ./make-release-tarball.nix { };

    pushStorageObject = callPackage ./push-storage-object.nix { };
  };

in fetchers // rec {
  # Library functions

  inherit (import ./dimension.nix) dimension;

  # A filter for removing packages that aren't supported on the current platform
  # according to 'meta.platforms'.
  platformFilterGeneric = system:
    # This needs to use the correct nixpkgs version so all the systems line up
    let
      platform = lib.systems.elaborate { inherit system; };
      # Can't just default to [] for platforms, since no meta.platforms
      # means "all platforms" not "no platforms"
    in drv:
    if drv ? meta && drv.meta ? platforms then
      lib.any (lib.meta.platformMatch platform) drv.meta.platforms
    else
      true;

  # Keep derivations and attrsets with 'recurseForDerivations'.
  # This ensures that we match the derivations that Hercules will see.
  filterDerivations = filterAttrsOnlyRecursive
    (n: attrs: lib.isDerivation attrs || attrs.recurseForDerivations or false);

  # A version of 'filterAttrsRecursive' that doesn't recurse into derivations.
  # This prevents us from going into an infinite loop with the 'out' attribute
  # on derivations.
  filterAttrsOnlyRecursive = pred: set:
    lib.listToAttrs (lib.concatMap (name:
      let v = set.${name};
      in if pred name v then
        [
          (lib.nameValuePair name
            (if builtins.isAttrs v && !lib.isDerivation v then
              filterAttrsOnlyRecursive pred v
            else
              v))
        ]
      else
        [ ]) (builtins.attrNames set));

  collectHaskellComponents = packages:
    let

      # These functions pull out from the Haskell package set either all the
      # components of a particular type, or all the checks.

      collectChecks = _: ps:
        recurseIntoAttrs (builtins.mapAttrs (_: p: p.checks) ps);

      collectComponents = type: ps:
        haskell-nix.haskellLib.collectComponents' type ps;

      # This computes the Haskell package set sliced by component type
    in recurseIntoAttrs (dimension "haskell" {
      # These names must match the subcomponent: components.<name>.<...>
      "library" = collectComponents;
      "tests" = collectComponents;
      "benchmarks" = collectComponents;
      "exes" = collectComponents;
      "checks" = collectChecks;
    } # Apply the selector to the Haskell package set
      (type: selector: (selector type) packages));
}
