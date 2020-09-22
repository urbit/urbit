{ lib
, stdenvNoCC
, runCommandLocal
, recurseIntoAttrs
, coreutils
, cacert
, curl
, google-cloud-sdk
, haskell-nix
, jq
, xxd
}:

import ./dimension.nix { inherit recurseIntoAttrs haskell-nix; } // rec {
  bootFakeShip = import ./boot-fake-ship.nix {
    inherit stdenvNoCC cacert;
  };

  testFakeShip = import ./test-fake-ship.nix {
    inherit stdenvNoCC cacert;
  };

  fetchGitHubLFS = import ./fetch-github-lfs.nix {
    inherit lib stdenvNoCC runCommandLocal cacert curl jq;
  };

  makeReleaseTarball = import ./make-release-tarball.nix {
    inherit lib stdenvNoCC coreutils;
  };

  pushStorageObject = import ./push-storage-object.nix {
    inherit lib stdenvNoCC coreutils google-cloud-sdk xxd;
  };

  # A filter for removing packages that aren't supported on the current platform
  # according to 'meta.platforms'.
  platformFilterGeneric = system:
    # This needs to use the correct nixpkgs version so all the systems line up
    let
      platform = lib.systems.elaborate { inherit system; };
    # Can't just default to [] for platforms, since no meta.platforms
    # means "all platforms" not "no platforms"
    in drv : if drv ? meta && drv.meta ? platforms then
      lib.any (lib.meta.platformMatch platform) drv.meta.platforms
    else true;

  # A version of 'filterAttrsRecursive' that doesn't recurse into
  # derivations. This prevents us from going into an infinite loop
  # with the 'out' attribute on derivations.  TODO: Surely this
  # shouldn't be necessary. I think normal 'filterAttrsRecursive' will
  # effectively cause infinite loops if you keep derivations and your
  # predicate forces the value of the attribute, as this then triggers
  # a loop on the 'out' attribute. Weird.
  filterAttrsOnlyRecursive = pred: set:
    lib.listToAttrs (
      lib.concatMap (name:
        let v = set.${name}; in
        if pred name v then [
          (lib.nameValuePair name (
            if builtins.isAttrs v && !lib.isDerivation v
              then filterAttrsOnlyRecursive pred v
              else v
          ))
        ] else []
      ) (builtins.attrNames set)
    );
} 
