{ lib, stdenv, darwin, haskell-nix, lmdb, gmp, zlib, libffi, brass
, enableStatic ? stdenv.hostPlatform.isStatic }:

haskell-nix.stackProject {
  compiler-nix-name = "ghc884";
  index-state = "2020-09-24T00:00:00Z";

  # This is incredibly difficult to get right, almost everything goes wrong.
  # See: https://github.com/input-output-hk/haskell.nix/issues/496
  src = haskell-nix.haskellLib.cleanSourceWith {
    # Otherwise this depends on the name in the parent directory, which
    # reduces caching, and is particularly bad on Hercules.
    # See: https://github.com/hercules-ci/support/issues/40
    name = "urbit-hs";
    src = ../../../pkg/hs;
  };

  modules = [{
    # This corresponds to the set of packages (boot libs) that ship with GHC.
    # We declare them yere to ensure any dependency gets them from GHC itself
    # rather than trying to re-install them into the package database.
    nonReinstallablePkgs = [
      "Cabal"
      "Win32"
      "array"
      "base"
      "binary"
      "bytestring"
      "containers"
      "deepseq"
      "directory"
      "filepath"
      "ghc"
      "ghc-boot"
      "ghc-boot-th"
      "ghc-compact"
      "ghc-heap"
      "ghc-prim"
      "ghci"
      "ghcjs-prim"
      "ghcjs-th"
      "haskeline"
      "hpc"
      "integer-gmp"
      "integer-simple"
      "mtl"
      "parsec"
      "pretty"
      "process"
      "rts"
      "stm"
      "template-haskell"
      "terminfo"
      "text"
      "time"
      "transformers"
      "unix"
      "xhtml"
    ];

    # Override various project-local flags and build configuration.
    packages = {
      urbit-king.patches = lib.optional stdenv.isDarwin [
        ../../pkgs/lmdb/darwin-fsync.patch
      ];
      urbit-king.components.exes.urbit-king = {
        enableStatic = enableStatic;
        enableShared = !enableStatic;

        configureFlags = lib.optionals enableStatic [
          "--ghc-option=-optl=-L${lmdb}/lib"
          "--ghc-option=-optl=-L${gmp}/lib"
          "--ghc-option=-optl=-L${libffi}/lib"
          "--ghc-option=-optl=-L${zlib}/lib"
        ] ++ lib.optionals (enableStatic && stdenv.isDarwin)
          [ "--ghc-option=-optl=-L${darwin.libiconv}/lib" ];

        postInstall = lib.optionalString (enableStatic && stdenv.isDarwin) ''
          find "$out/bin" -type f -exec \
            install_name_tool -change \
              ${stdenv.cc.libc}/lib/libSystem.B.dylib \
              /usr/lib/libSystem.B.dylib {} \;
        '';
      };

      urbit-king.components.tests.urbit-king-tests.testFlags =
        [ "--brass-pill=${brass.lfs}" ];
    };
  }];
}

