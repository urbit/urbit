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
  libLocal = callPackage ./nix/lib { };

  # The key with google storage bucket write permission,
  # deployed to ci via nixops deployment.keys."service-account.json".
  serviceAccountKey = builtins.readFile
    ("/var/lib/hercules-ci-agent/secrets/service-account.json");

  # Filter out systems that this machine does not support.
  systems = lib.filterAttrs (_: v: builtins.elem v supportedSystems) {
    linux = "x86_64-linux";
    darwin = "x86_64-darwin";
  };

  # Build the ci matrix for each of the supported systems, see pkgsFinal
  # for the total set of attributes that will be evaluated per system.
in libLocal.dimension "system" systems (systemName: system:
  let
    pkgsShared = import ./default.nix {
      inherit system;

      enableStatic = false;
    };

    pkgsStatic = import ./default.nix {
      inherit system;

      enableStatic = true;
    };

    # Filter the stack project to only our locally declared packages.
    pkgsHaskell = haskell-nix.haskellLib.selectProjectPackages pkgsStatic.hs;

    # The top-level set of attributes to build on ci.
    pkgsFinal = {
      # Expose select packages to increase signal-to-noise of the ci dashboard.
      inherit (pkgsStatic) urbit;
      inherit (pkgsShared) urbit-tests;

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
      hs = libLocal.collectHaskellComponents pkgsHaskell;

      # Push the tarball to the google storage bucket for the current platform.
      release = let inherit (pkgsStatic) tarball;
      in libLocal.pushStorageObject {
        inherit serviceAccountKey;

        bucket = "bootstrap.urbit.org";
        object = "ci/${lib.removePrefix "/nix/store/" (toString tarball)}";
        name = tarball.name;
        file = tarball.out;
        contentType = "application/x-gtar";
      };
    };

    # Filter derivations that have meta.platform missing the current system,
    # such as testFakeShip on darwin.
    platformFilter = libLocal.platformFilterGeneric system;

  in libLocal.filterAttrsOnlyRecursive (_: v: platformFilter v) pkgsFinal)
