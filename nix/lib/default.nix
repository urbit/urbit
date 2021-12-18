# Functions that are expected run on the native (non-cross) system.

{ callPackage }:

rec {
  bootFakeShip = callPackage ./boot-fake-ship.nix { };

  testFakeShip = callPackage ./test-fake-ship.nix { inherit bootFakeShip; };

  fetchGitHubLFS = callPackage ./fetch-github-lfs.nix { };

  makeReleaseTarball = callPackage ./make-release-tarball.nix { };
}
