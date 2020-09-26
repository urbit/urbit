/* 

Examples

  Native shared urbit binary:

    $ nix-build -A urbit

  Static urbit binary:

    $ nix-build -A urbit \
        --argstr crossSystem x86_64-unknown-linux-musl

  Static urbit-king binary:

    $ nix-build -A hs.urbit-king.components.exes.urbit-king \
        --argstr crossSystem x86_64-unknown-linux-musl

  Build a specific Haskell package from ./pkg/hs:

    $ nix-build -A hs.urbit-noun.components.library
    $ nix-build -A hs.urbit-atom.components.benchmarks.urbit-atom-bench
    $ nix-build -A hs.urbit-atom.components.tests.urbit-atom-tests

*/

{ system ? builtins.currentSystem
, crossSystem ? null
, sourcesOverride ? { }
, configOverride ? { }
, overlaysOverride ? [ ]

# Whether to use `pkgs.pkgsStatic.*` to obtain statically linked package
# dependencies - ie. when building fully-static libraries or executables. 
, withStatic ? crossSystem != null
}:

let

  pkgs = import ./nix/pkgs.nix {
    inherit system crossSystem sourcesOverride configOverride overlaysOverride;
  };

  # Enrich the global package set with our local functions and packages.
  callPackage = pkgs.lib.callPackageWith (hostPackages // localLib // localPackages);

  # Utilise nixpkgs's top-level/static.nix overlay if required.
  hostPackages = if withStatic then pkgs.pkgsStatic else pkgs;

  # Local library import from derivation functions such as `fetchGitHubLFS`, etc.
  # upon which local package defintions are dependent.
  localLib = pkgs.callPackage ./nix/lib { };

  # Local vendored packages defined in `./pkg`.
  # For non-vendored nixpkgs specific package overrides, see `./nix/overlays`.
  localPackages = {
    argon2u = callPackage ./pkg/argon2u { };

    ca-bundle = callPackage ./pkg/ca-bundle { };

    ed25519 = callPackage ./pkg/ed25519 { };

    ent = callPackage ./pkg/ent { };

    ge-additions = callPackage ./pkg/ge-additions { };

    libaes-siv = callPackage ./pkg/libaes-siv { };

    libscrypt = callPackage ./pkg/libscrypt { };

    murmur3 = callPackage ./pkg/murmur3 { };

    softfloat3 = callPackage ./pkg/softfloat3 { };

    herb = callPackage ./pkg/herb { inherit (pkgs) python; };

    arvo = callPackage ./pkg/arvo { };

    ivory = callPackage ./pkg/pill/ivory.nix { };

    brass = callPackage ./pkg/pill/brass.nix { };

    solid = callPackage ./pkg/pill/solid.nix { };

    urbit = callPackage ./pkg/urbit { };

    hs = callPackage ./pkg/hs { inherit (pkgs) haskell-nix; };
  };

  # Additional top-level package attributes exposed for convenience.
  extraPackages = with localPackages; {
    urbit-debug = urbit.override { withDebug = true; };

    urbit-tests = localLib.testFakeShip {
      inherit urbit herb;

      pill = solid.lfs;
    };

    ivory-ropsten = ivory.override { arvo = arvo.ropsten; };
    brass-ropsten = brass.override { arvo = arvo.ropsten; };
  };

  checkPlatform =
    if withStatic && hostPackages.stdenv.hostPlatform.libc == "glibc"
      then builtins.trace "[1;31mwarning: statically linking against glibc.[0m"
      else pkgs.lib.id;

in checkPlatform (localPackages // extraPackages)
