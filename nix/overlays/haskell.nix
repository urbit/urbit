final: prev:
let
  compiler-nix-name = "ghc884";
  project = prev.haskell-nix.stackProject {
    compiler-nix-name = "ghc884";
    index-state = "2020-09-24T00:00:00Z";

    # This is incredibly difficult to get right, almost everything goes wrong.
    # See: https://github.com/input-output-hk/haskell.nix/issues/496
    src = prev.haskell-nix.haskellLib.cleanSourceWith {
      # Otherwise this depends on the name in the parent directory, which
      # reduces caching, and is particularly bad on Hercules.
      # See: https://github.com/hercules-ci/support/issues/40
      name = "urbit-hs";
      src = ../../pkg/hs;
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
        urbit-king.components.exes.urbit-king = {
          enableStatic = prev.enableStatic;
          enableShared = !prev.enableStatic;

          configureFlags = prev.lib.optionals prev.enableStatic [
            "--ghc-option=-optl=-L${prev.lmdb}/lib"
            "--ghc-option=-optl=-L${prev.gmp}/lib"
            "--ghc-option=-optl=-L${prev.libffi}/lib"
            "--ghc-option=-optl=-L${prev.zlib}/lib"
          ] ++ prev.lib.optionals (prev.enableStatic && prev.stdenv.isDarwin)
            [ "--ghc-option=-optl=-L${prev.darwin.libiconv}/lib" ];

          postInstall = prev.lib.optionalString (prev.enableStatic && prev.stdenv.isDarwin) ''
          find "$out/bin" -type f -exec \
            install_name_tool -change \
              ${prev.stdenv.cc.libc}/lib/libSystem.B.dylib \
              /usr/lib/libSystem.B.dylib {} \;
        '';
        };

        urbit-king.components.tests.urbit-king-tests.testFlags =
          [ "--brass-pill=${prev.brass.lfs}" ];

        lmdb.components.library.libs = prev.lib.mkForce [ prev.lmdb ];
      };
    }];
  };
  hls = import prev.sources.nix-haskell-hls {
    config = {
      ghcVersion = compiler-nix-name;

      hls.unstable = true;

      haskell-nix.checkMaterialization = false;
      haskell-nix.nixpkgs-pin = "nixpkgs-2009";
      haskell-nix.hackage.index = {
        state = project.index-state;
        sha256 =
          "f8ef4679f4c3f0c7a075dfef715f1db1b51e3934dc28cef9b3f83b2e1f1e2f77";
      };
    };
  };
in {
  tools = [
      hls.hls-renamed
      hls.hls-wrapper-nix
      hls.implicit-hie
  ];
}
