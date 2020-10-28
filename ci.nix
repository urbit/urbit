/* Examples

   Perform the same evaluation that occurs on CI via:

     $ NIX_PATH="" nix-instantiate ci.nix --arg supportedSystems '["x86_64-darwin"]'

   Build the release tarball:

     $ NIX_PATH="" nix-instantiate ci.nix -A darwin.tarball
*/

{ supportedSystems ? [ "x86_64-linux" "x86_64-darwin" ] }:

let

  inherit (import ./nix/default.nix { }) lib haskell-nix callPackage;

  # Local library import from derivation functions such as fetchGitHubLFS, etc.
  # upon which local package defintions are dependent.
  localLib = callPackage ./nix/lib { };

  # The key with google storage bucket write permission,
  # deployed to ci via nixops `deployment.keys."service-account.json"`.
  serviceAccountKey = builtins.readFile ("/var/run/keys/service-account.json");

  # Push a split output derivation containing "out" and "hash" outputs.
  pushObject =
    { name, extension, drv, contentType ? "application/octet-stream" }:
    let
      # Use the sha256 for the object key prefix.
      sha256 = builtins.readFile (drv.hash + "/sha256");
      # Use md5 as an idempotency check for gsutil.
      contentMD5 = builtins.readFile (drv.hash + "/md5");
    in localLib.pushStorageObject {
      inherit serviceAccountKey name contentMD5 contentType;

      bucket = "bootstrap.urbit.org";
      object = "ci/${lib.removeSuffix extension name}.${sha256}.${extension}";
      file = drv.out;
    };

  # Build and push a split output pill derivation with the ".pill" file extension.
  pushPill = name: pill:
    pushObject {
      inherit name;

      drv = pill.build;
      extension = "pill";
    };

  systems = lib.filterAttrs (_: v: builtins.elem v.system supportedSystems) {
    linux = {
      system = "x86_64-linux";
      crossSystem = lib.systems.examples.musl64;
    };

    darwin = {
      system = "x86_64-darwin";
      crossSystem = null;
    };
  };

in localLib.dimension "system" systems (systemName:
  { system, crossSystem }:
  let
    # Check the pinned haskell.nix hashes for correctness.
    checkMaterialization = true;

    # Shared libraries/executables for the build (current) system.
    localPackages = import ./default.nix {
      inherit system checkMaterialization;

      enableStatic = false;
    };

    # Static libraries/executables for the host (cross) system.
    staticPackages = import ./default.nix {
      inherit system crossSystem checkMaterialization;

      enableStatic = true;
    };

    # Filter the stack project to only our locally declared packages.
    haskellPackages =
      haskell-nix.haskellLib.selectProjectPackages staticPackages.hs;

    # The top-level set of attributes to build on ci.
    finalPackages = localPackages // rec {
      # Expose the nix-shell derivation as a sanity check.
      shell = import ./shell.nix;

      # Replace the top-level urbit attribute with the static variant.
      urbit = staticPackages.urbit;

      # Replace the top-level tarball attribute with the static variant.
      tarball = staticPackages.tarball;

      # Replace the localPackages.hs attribute with the individual components
      # displayed as top-level attributes:
      #
      # <system>.hs.library.[...]
      # <system>.hs.tests.[...]
      # <system>.hs.bencharmks.[...]
      # ...
      hs = localLib.collectHaskellComponents haskellPackages;

      # Push the tarball to the remote google storage bucket.
      release = pushObject {
        name = tarball.name;
        extension = tarball.meta.extension;
        contentType = "application/x-gtar";
        drv = tarball;
      };

      # Replace top-level pill attributes with push to google storage variants.
    } // lib.optionalAttrs (system == "x86_64-linux") {
      ivory = pushPill "ivory" localPackages.ivory;
      brass = pushPill "brass" localPackages.brass;
      solid = pushPill "solid" localPackages.solid;

      ivory-ropsten = pushPill "ivory-ropsten" localPackages.ivory-ropsten;
      brass-ropsten = pushPill "brass-ropsten" localPackages.brass-ropsten;
    };

    # Filter derivations that have meta.platform missing the current system,
    # such as testFakeShip on darwin.
    platformFilter = localLib.platformFilterGeneric system;

  in localLib.filterAttrsOnlyRecursive (_: v: platformFilter v) finalPackages)
