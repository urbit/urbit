
/* Examples

   Shared urbit and urbit-worker binaries:

     $ nix-build -A urbit

   Static urbit and urbit-worker binaries:

     $ nix-build -A urbit --arg enableStatic true

   Note that on linux the previous command is equivalent to:

     $ nix-build -A urbit --argstr crossSystem x86_64-unknown-linux-musl \
                          --arg enableStatic true

   Static release tarball:

     $ nix-build -A tarball --arg enableStatic true

   Build a pill:

     $ nix-build -A ivory.build
     $ nix-build -A brass.build
     $ nix-build -A solid.build

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

  pkgsTopLevel = import ./nix/default.nix { inherit system; };

  pkgsNative = if pkgsTopLevel.stdenv.isDarwin
               then pkgsTopLevel.llvmPackages_9
               else pkgsTopLevel;

  pkgsCross = import ./nix/default.nix {
    inherit system sources config overlays crossOverlays;

    # If we're running on linux and crossSystem is unspecified but
    # enableStatic = true - set the crossSystem to musl64.
    crossSystem =
      if system == "x86_64-linux" && crossSystem == null && enableStatic then
        "x86_64-unknown-linux-musl"
      else
        crossSystem;
  };

  # Use nixpkgs' top-level/static overlay if enableStatic = true.
  pkgsStatic = if enableStatic then pkgsCross.pkgsStatic else pkgsCross;

  # Enrich the global package set with our local functions and packages.
  # Cross vs static build dependencies can be selectively overridden for
  # inputs like python etc.
  callPackage =
    pkgsTopLevel.lib.callPackageWith (pkgsStatic // libLocal // pkgsLocal);

  # Local library import-from-derivation functions such as fetchGitHubLFS, etc.
  libLocal = pkgsNative.callPackage ./nix/lib { };

  # Local vendored packages defined in ./pkg.
  # For non-vendored nixpkgs specific package overrides, see ./nix/overlays.
  pkgsLocal = {
    ca-bundle = callPackage ./nix/pkgs/ca-bundle { };

    ent = callPackage ./nix/pkgs/ent { };

    libaes_siv = callPackage ./nix/pkgs/libaes_siv { inherit (pkgsNative) cmake; };

    murmur3 = callPackage ./nix/pkgs/murmur3 { };

    softfloat3 = callPackage ./nix/pkgs/softfloat3 { };

    herb = callPackage ./nix/pkgs/herb { inherit (pkgsCross) python; };

    arvo = callPackage ./nix/pkgs/arvo { };

    ivory = callPackage ./nix/pkgs/pill/ivory.nix { };

    brass = callPackage ./nix/pkgs/pill/brass.nix { };

    solid = callPackage ./nix/pkgs/pill/solid.nix { };

    marsSources = callPackage ./nix/pkgs/marsSources { };

    urbit = callPackage ./nix/pkgs/urbit { inherit enableStatic; };

    urcrypt = callPackage ./nix/pkgs/urcrypt { inherit enableStatic; };

    docker-image = callPackage ./nix/pkgs/docker-image { };
  };

  # Additional top-level packages and attributes exposed for convenience.
  pkgsExtra = with pkgsLocal; rec {
    # Expose packages with local customisations (like patches) for dev access.
    inherit (pkgsStatic) libsigsegv lmdb;

    urbit-debug = urbit.override { enableDebug = true; };
    urbit-tests = libLocal.testFakeShip {
      inherit herb;
      inherit arvo;

      urbit = urbit-debug;
      pill = solid.lfs;
    };

    ivory-ropsten = ivory.override { arvo = arvo.ropsten; };
    brass-ropsten = brass.override { arvo = arvo.ropsten; };

    # Create a .tgz of the primary binaries.
    tarball = let
      name = "urbit-v${urbit.version}-${urbit.system}";
    in libLocal.makeReleaseTarball {
      inherit name;

      contents = {
        "${name}/urbit" = "${urbit}/bin/urbit";
        "${name}/urbit-worker" = "${urbit}/bin/urbit-worker";
      };
    };

    inherit (pkgsNative) skopeo;

    # A convenience function for constructing a shell.nix for any of the
    # pkgsLocal derivations by automatically propagating any dependencies 
    # to the nix-shell.
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
      pkgsNative.mkShell ({
        inputsFrom = packages pkgsLocal;
      } // builtins.removeAttrs attrs [ "packages" ]);
  };

  # Ensure that in the case of cross-compilation we're not statically linking
  # against glibc. This is typically a sign that crossSystem is misconfigured.
  checkPlatform =
    if enableStatic && pkgsCross.stdenv.hostPlatform.libc == "glibc" then
      builtins.trace "warning: statically linking against glibc."
    else
      pkgsTopLevel.lib.id;

in checkPlatform (pkgsLocal // pkgsExtra)
