{ lib
, stdenvNoCC
, runCommandLocal
, coreutils
, cacert
, curl
, google-cloud-sdk
, jq
, xxd
}:

{
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
} 
