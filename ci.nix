/* Examples

   Perform the same evaluation that occurs on CI via:

     $ NIX_PATH="" nix-instantiate ci.nix --arg supportedSystems '["x86_64-darwin"]'

   Build the release tarball:

     $ NIX_PATH="" nix-instantiate ci.nix -A darwin.tarball
*/

{ supportedSystems ? [ "x86_64-linux" "x86_64-darwin" ] }:

let

  inherit (import ./nix/default.nix { })
    lib haskell-nix recurseIntoAttrs callPackage;

  # Local library import from derivation functions such as fetchGitHubLFS, etc.
  # upon which local package defintions are dependent.
  localLib = callPackage ./nix/lib { };

  # The key with google storage bucket write permission,
  # deployed to ci via nixops `deployment.keys."service-account.json"`.
  serviceAccountKey = builtins.readFile
    ("/var/lib/hercules-ci-agent/secrets/service-account.json");

  # Push a derivation to a remote storage bucket as a post-build effect.
  pushObject = { name, drv, contentType ? "application/octet-stream" }:
    localLib.pushStorageObject {
      inherit name contentType serviceAccountKey;

      bucket = "bootstrap.urbit.org";
      object = "ci/${lib.removePrefix "/nix/store/" (toString drv)}";
      file = drv.out;
    };

  # # Build and push a split output pill derivation with the ".pill" file extension.
  # pushPill = name: pill:
  #   pushObject {
  #     inherit name;

  #     drv = pill.build;
  #     extension = "pill";
  #   };

  systems = lib.filterAttrs (_: v: builtins.elem v supportedSystems) {
    linux = "x86_64-linux";
    darwin = "x86_64-darwin";
  };

in localLib.dimension "system" systems (systemName: system:
  let
    dynamicPackages = import ./default.nix {
      inherit system;

      enableStatic = false;
    };

    staticPackages = import ./default.nix {
      inherit system;

      enableStatic = true;
    };

    # Filter the stack project to only our locally declared packages.
    haskellPackages =
      haskell-nix.haskellLib.selectProjectPackages staticPackages.hs;

    # The top-level set of attributes to build on ci.
    finalPackages = {
      # Expose select packages to increase signal-to-noise of the ci dashboard.
      inherit (staticPackages) urbit;
      inherit (dynamicPackages) urbit-tests;

      # Expose the nix-shell derivation as a sanity check + possible cache hit.
      shell = import ./shell.nix;

      # Replace the .hs attribute with the individual collections of components
      # displayed as top-level attributes:
      #
      # <system>.hs.library.[...]
      # <system>.hs.checks.[...]
      # <system>.hs.tests.[...]
      # <system>.hs.benchmarks.[...]
      # ...
      #
      # Note that .checks are the actual _execution_ of the tests.
      hs = localLib.collectHaskellComponents haskellPackages;

      # Push the tarball to the google storage bucket for the current platform.
      release = let inherit (staticPackages) tarball;
      in pushObject {
        name = tarball.name;
        drv = tarball;
        contentType = "application/x-gtar";
      };
    };

    # Filter derivations that have meta.platform missing the current system,
    # such as testFakeShip on darwin.
    platformFilter = localLib.platformFilterGeneric system;

  in localLib.filterAttrsOnlyRecursive (_: v: platformFilter v) finalPackages)
