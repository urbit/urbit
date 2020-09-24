{ stdenv, haskell-nix, gmp, zlib }:

let

  staticFlags = stdenv.lib.optionals stdenv.hostPlatform.isStatic [
    "--disable-executable-dynamic"
    "--disable-shared"
    "--ghc-option=-optl=-pthread"
    "--ghc-option=-optl=-static"
    "--ghc-option=-optl=-L${gmp.override { withStatic = true; }}/lib"
    "--ghc-option=-optl=-L${zlib.override { shared = false; static = true; }}/lib"
  ];

in haskell-nix.stackProject {
  index-state = "2020-09-24T00:00:00Z";

  src = haskell-nix.cleanSourceHaskell {
    name = "urbit-hs";
    src = ./.;
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

    packages = {
      # Disable the urbit-king test-suite for now - since it relies
      # on relative paths to lfs pills.
      urbit-king.doCheck = false;
      urbit-king.components.exes.urbit-king.configureFlags = staticFlags;
    };
  }];
}
