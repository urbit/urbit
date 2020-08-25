{ pkgs }:

let

  staticFlagsIfMusl = 
    pkgs.stdenv.lib.optionals pkgs.stdenv.hostPlatform.isMusl ([
      "--disable-executable-dynamic"
      "--disable-shared"
      "--ghc-option=-optl=-pthread"
      "--ghc-option=-optl=-static"
      "--ghc-option=-optl=-L${pkgs.gmp6.override { withStatic = true; }}/lib"
      "--ghc-option=-optl=-L${pkgs.zlib}/lib"
    ]);
  
  project = pkgs.haskell-nix.stackProject {
    src = pkgs.haskell-nix.cleanSourceHaskell {
      name = "urbit-king";
      src = ./.;
    };

    modules = [{ 
      # This corresponds to the set of packages (boot libs) that ship with GHC.
      # We declare them here to ensure any dependency gets them from GHC iteself
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

      packages = { 
        # Disable the urbit-king test-suite for now - since it relies
        # relative paths to lfs pills.
        urbit-king.doCheck = false;
 
        urbit-king.components.exes.urbit-king.configureFlags =
          staticFlagsIfMusl;
      };
    }];
  };

in project
