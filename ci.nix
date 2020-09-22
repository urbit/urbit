/* 

Examples

  Perform the same evaluation that occurs on Hercules CI:

    $ NIX_PATH="" nix-instantiate ci.nix

*/

{ supportedSystems ? [ "x86_64-linux" "x86_64-darwin" ] }:

let

  pkgs = import ./nix/pkgs.nix { };

  inherit (pkgs.callPackage ./nix/lib { })
    dimensionWith
    dimension
    dimensionHaskell
    makeReleaseTarball
    pushStorageObject
    platformFilterGeneric
    filterAttrsOnlyRecursive;

  # The key with google storage bucket write permission,
  # deployed to ci via nixops `deployment.keys."service-account.json"`.
  serviceAccountKey = builtins.readFile ("/var/run/keys/service-account.json");

  # Sources are expected to be a split-derivation containing "out" and "md5".
  pushObject = name: extension: src:
    let md5 = builtins.readFile src.md5;
    in pushStorageObject {
      inherit serviceAccountKey md5;

      src = src.out;
      bucket = "tlon-us-terraform";
      object = "releases/${name}-${md5}.${extension}";
    };

  # Push a pill with the ".build" file extension.
  pushPill = name: src: pushObject name "pill" src.build;

  makeTarball = ps: makeReleaseTarball {
    name = "urbit";
    contents = {
      "urbit" = "${ps.urbit}/bin/urbit";
      "urbit-worker" = "${ps.urbit}/bin/urbit-worker";
      "urbit-king" = "${ps.hs.urbit-king.components.exes.urbit-king}/bin/urbit-king";
    };
  };

  systems = pkgs.lib.filterAttrs (_: v: builtins.elem v supportedSystems) {
    linux = "x86_64-linux";
    darwin = "x86_64-darwin";
  };

in dimensionWith "system" systems (systemName: system:

  let

    # Instantiate shared and static libraries/executables for the specific system.
    sharedPackages = import ./default.nix { inherit system; };
    staticPackages = import ./default.nix {
      inherit system;

      withStatic = true;
      crossSystem =
        if pkgs.stdenv.isLinux
          then pkgs.lib.systems.examples.musl64
          else null;
    };

    # Filter out attributes we don't want to recurse into for ci.
    #
    # We remove Haskell packages/projects and instead use the `haskellProject`
    # function from above to force evaluation via `recurseIntoAttrs` and to better
    # display the individual components instead of all the haskell-nix attributes.
    extraPackages = {
      urbit = staticPackages.urbit;

      hs = dimensionHaskell "haskell" staticPackages.hs;

      # Push various artefacts to the remote storage bucket.
      release = dimension "release" {
        tarball = pushObject "urbit-${system}" "tar.gz" (makeTarball staticPackages);

        ivory = pushPill "ivory" sharedPackages.ivory;
        brass = pushPill "brass" sharedPackages.brass;
        solid = pushPill "solid" sharedPackages.solid;

        ivory-ropsten = pushPill "ivory-ropsten" sharedPackages.ivory-ropsten;
        brass-ropsten = pushPill "brass-ropsten" sharedPackages.brass-ropsten;
      };
    };

  in filterAttrsOnlyRecursive (_: v: platformFilterGeneric system v) (sharedPackages // extraPackages)
)
