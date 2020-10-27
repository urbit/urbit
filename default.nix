/* Examples

   Shared urbit and urbit-worker binaries:

     $ nix-build -A urbit

   Static urbit and urbit-worker binaries:

     $ nix-build -A urbit --arg enableSatic true

   Static urbit-king binary:

     $ nix-build -A hs.urbit-king.components.exes.urbit-king --arg enableStatic true

   Static release tarball:

     $ nix-build -A tarball --arg enableStatic true

   Build a pill:

     $ nix-build -A ivory.build
     $ nix-build -A brass.build
     $ nix-build -A solid.build

   Build a specific Haskell package from ./pkg/hs:

     $ nix-build -A hs.urbit-noun.components.library
     $ nix-build -A hs.urbit-atom.components.benchmarks.urbit-atom-bench
     $ nix-build -A hs.urbit-atom.components.tests.urbit-atom-tests
*/

# The build system where packages will be _built_.
{ system ? builtins.currentSystem
  # The host system where packages will _run_.
, crossSystem ? null
  # Additional sources.json overrides.
, sources ? { }
  # Additional nixpkgs.config overrides.
, config ? { }
  # Additional nixpkgs.overlays.
, overlays ? [ ]
  # Overlays to apply to the last package set in cross compilation.
, crossOverlays ? [ ]
  # Whether to use pkgs.pkgsStatic.* to obtain statically linked package
  # dependencies - ie. when building fully-static libraries or executables. 
, enableStatic ? crossSystem != null
  # Whether to check that the pinned hashes for haskell.nix are correct.
  # Set to false by default since it's a lot slower, but true for ci.
, checkMaterialization ? false }:

let

  pkgs = import ./nix/default.nix {
    inherit system crossSystem sources config overlays crossOverlays;
  };

  # Local library import from derivation functions such as fetchGitHubLFS, etc.
  # upon which local package defintions are dependent.
  localLib = pkgs.callPackage ./nix/lib { };

  # Utilise nixpkgs's top-level/static.nix overlay if required.
  hostPackages = if enableStatic then pkgs.pkgsStatic else pkgs;

  # Enrich the global package set with our local functions and packages.
  callPackage =
    pkgs.lib.callPackageWith (hostPackages // localLib // localPackages);

  # Local vendored packages defined in ./pkg.
  # For non-vendored nixpkgs specific package overrides, see ./nix/overlays.
  localPackages = {
    argon2u = callPackage ./nix/pkgs/argon2u { };

    ca-bundle = callPackage ./nix/pkgs/ca-bundle { };

    ed25519 = callPackage ./nix/pkgs/ed25519 { };

    ent = callPackage ./nix/pkgs/ent { };

    ge-additions = callPackage ./nix/pkgs/ge-additions { };

    libaes_siv = callPackage ./nix/pkgs/libaes_siv { };

    libscrypt = callPackage ./nix/pkgs/libscrypt { };

    murmur3 = callPackage ./nix/pkgs/murmur3 { };

    softfloat3 = callPackage ./nix/pkgs/softfloat3 { };

    herb = callPackage ./nix/pkgs/herb { inherit (pkgs) python; };

    arvo = callPackage ./nix/pkgs/arvo { };

    ivory = callPackage ./nix/pkgs/pill/ivory.nix { };

    brass = callPackage ./nix/pkgs/pill/brass.nix { };

    solid = callPackage ./nix/pkgs/pill/solid.nix { };

    urbit = callPackage ./nix/pkgs/urbit { inherit enableStatic; };

    hs = callPackage ./nix/pkgs/hs {
      inherit (pkgs) haskell-nix;
      inherit checkMaterialization enableStatic;
    };
  };

  # Additional top-level packages and attributes exposed for convenience.
  extraPackages = with localPackages; {
    # Expose packages we've local customisations for.
    inherit (hostPackages) libsigsegv;

    urbit-debug = urbit.override { enableDebug = true; };
    urbit-tests = localLib.testFakeShip {
      inherit urbit herb;

      pill = solid.lfs;
    };

    ivory-ropsten = ivory.override { arvo = arvo.ropsten; };
    brass-ropsten = brass.override { arvo = arvo.ropsten; };

    # FIXME: tarball binaries need executable permissions set?

    # Create a .tar.gz of the primary binaries.
    tarball = localLib.makeReleaseTarball {
      name = "urbit-tarball";
      contents = {
        "urbit" = "${urbit}/bin/urbit";
        "urbit-worker" = "${urbit}/bin/urbit-worker";
        "urbit-king" =
          "${hs.urbit-king.components.exes.urbit-king}/bin/urbit-king";
      };
    };

    # A convenience function for constructing a shell.nix for any of the
    # localPackage derivations by automatically propagating any dependencies such
    # as buildInputs to the nix-shell.
    #
    # Example:
    #
    #  let
    #    pkgs = import ./default.nix { };
    #  in pkgs.shellFor {
    #    packages = ps: [
    #      ps.urbit
    #      ps.herb
    #    ];
    #  }
    #
    shellFor = { name, packages, ... }@attrs:
      pkgs.mkShell ({
        inputsFrom = packages localPackages;
      } // builtins.removeAttrs attrs [ "packages" ]);
  };

  # Ensure that in the case of cross-compilation we're not statically linking
  # against glibc. This is typically a sign that crossSystem is misconfigured.
  checkPlatform =
    if enableStatic && hostPackages.stdenv.hostPlatform.libc == "glibc" then
      builtins.trace "warning: statically linking against glibc."
    else
      pkgs.lib.id;

in checkPlatform (localPackages // extraPackages)
