/* 

Examples

  Perform the same evaluation that occurs on Hercules CI:

    $ NIX_PATH="" nix-instantiate ci.nix

*/
{ system ? builtins.currentSystem }:

let

  pkgs = import ./nix/pkgs.nix { inherit system; };

  localLib = pkgs.callPackage ./nix/lib { };

  sharedPackages = import ./default.nix { inherit system; };
  staticPackages = import ./default.nix {
    inherit system;

    withStatic = true;
    crossSystem =
      if pkgs.stdenv.isLinux
        then pkgs.lib.systems.examples.musl64
        else null;
  };

  # These functions pull out from the Haskell package all the components of
  # a particular type - which ci will then build as top-level attributes.
  haskellProject = project:
    let
      collectChecks = _: xs:
        pkgs.recurseIntoAttrs (builtins.mapAttrs (_: x: x.checks) xs);

      collectComponents = type: xs:
        pkgs.haskell-nix.haskellLib.collectComponents' type xs;

      packages = pkgs.haskell-nix.haskellLib.selectProjectPackages project;

    # This computes the Haskell package set sliced by component type - these
    # are then displayed as the haskell build attributes in hercules ci.
    in pkgs.recurseIntoAttrs (localLib.dimensionWith "haskell" {
      library = collectComponents;
      tests = collectComponents;
      benchmarks = collectComponents;
      exes = collectComponents;
      checks = collectChecks;
    } (type: selector: (selector type) packages));

  # The key with google storage bucket write permission,
  # deployed to ci via nixops `deployment.keys."service-account.json"`.
  serviceAccountKey = builtins.readFile ("/var/run/keys/service-account.json");

  # Sources are expected to be a split-derivation containing "out" and "md5".
  pushObject = name: extension: src:
    let md5 = builtins.readFile src.md5;
    in localLib.pushStorageObject {
      inherit serviceAccountKey md5;

      src = src.out;
      bucket = "tlon-us-terraform";
      object = "releases/${name}-${md5}.${extension}";
    };

  # Push a pill with the ".build" file extension.
  pushPill = name: src: pushObject name "pill" src.build;

  makeTarball = ps: localLib.makeReleaseTarball {
    name = "urbit";
    contents = {
      "urbit" = "${ps.urbit}/bin/urbit";
      "urbit-worker" = "${ps.urbit}/bin/urbit-worker";
      "urbit-king" = "${ps.hs.urbit-king.components.exes.urbit-king}/bin/urbit-king";
    };
  };

  # Filter out attributes we don't want to recurse into for ci.
  #
  # We remove Haskell packages/projects and instead use the `haskellProject`
  # function from above to force evaluation via `recurseIntoAttrs` and to better
  # display the individual components instead of all the haskell-nix attributes.
  extraPackages = {
    urbit = staticPackages.urbit;

    hs = haskellProject staticPackages.hs;

    # Push various artefacts to the remote storage bucket.
    release = localLib.dimension "release" {
      tarball = pushObject "urbit" "tar.gz" (makeTarball staticPackages);

      ivory = pushPill "ivory" sharedPackages.ivory;
      brass = pushPill "brass" sharedPackages.brass;
      solid = pushPill "solid" sharedPackages.solid;

      ivory-ropsten = pushPill "ivory-ropsten" sharedPackages.ivory-ropsten;
      brass-ropsten = pushPill "brass-ropsten" sharedPackages.brass-ropsten;
    };
  };

in localLib.dimension "system" (sharedPackages // extraPackages)
