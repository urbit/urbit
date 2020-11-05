/* Examples

   Shared urbit and urbit-worker binaries:

     $ nix-build -A urbit

   Static urbit and urbit-worker binaries:

     $ nix-build -A urbit --arg enableSatic true

   Note that on linux the previous command is equivalent to:

     $ nix-build -A urbit --argstr crossSystem x86_64-unknown-linux-musl \
                          --arg enableSatic true

   Static urbit-king binary:

     $ nix-build -A hs.urbit-king.components.exes.urbit-king --arg enableStatic true

   Static release tarball:

     $ nix-build -A tarball --arg enableStatic true

   Build a pill:

     $ nix-build -A ivory.build
     $ nix-build -A brass.build
     $ nix-build -A solid.build

   Run the king-haskell tests:

     $ nix-build -A hs.urbit-king.checks.urbit-king-tests

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
, enableStatic ? false }:

let

  pkgs = import ./nix/default.nix {
    inherit system sources config overlays crossOverlays;

    crossSystem =
      # If we're running on linux and crossSystem is unspecified but static
      # builds are requested - set the crossSystem to musl64.
      if system == "x86_64-linux" && crossSystem == null && enableStatic then
        "x86_64-unknown-linux-musl"
      else
        crossSystem;
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
      inherit enableStatic;
    };
  };

  # Additional top-level packages and attributes exposed for convenience.
  extraPackages = with localPackages; rec {
    # Expose packages we've local customisations for.
    inherit (hostPackages) libsigsegv;

    urbit-debug = urbit.override { enableDebug = true; };
    urbit-tests = localLib.testFakeShip {
      inherit herb;

      urbit = urbit-debug;
      pill = solid.lfs;
    };

    ivory-ropsten = ivory.override { arvo = arvo.ropsten; };
    brass-ropsten = brass.override { arvo = arvo.ropsten; };

    # FIXME: tarball binaries need executable permissions set?

    # Create a .tgz of the primary binaries.
    tarball = let
      name = "urbit-v${urbit.version}-${urbit.system}";
      urbit-king = hs.urbit-king.components.exes.urbit-king;
    in localLib.makeReleaseTarball {
      inherit name;

      contents = {
        "${name}/urbit" = "${urbit}/bin/urbit";
        "${name}/urbit-worker" = "${urbit}/bin/urbit-worker";
        "${name}/urbit-king" = "${urbit-king}/bin/urbit-king";
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
