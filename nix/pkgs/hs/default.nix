{ lib, stdenv, darwin, haskell-nix, gmp, zlib, libffi
, enableStatic ? stdenv.hostPlatform.isStatic }:

let

  compiler-nix-name = "ghc884";
  index-state = "2020-09-24T00:00:00Z";

  project = haskell-nix.stackProject {
    inherit compiler-nix-name index-state;

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

      packages = {
        # Disable the urbit-king test-suite for now - since it relies
        # on relative paths to lfs pills.
        urbit-king.doCheck = false;
        urbit-king.components.exes.urbit-king.enableStatic = enableStatic;
        urbit-king.components.exes.urbit-king.enableShared = !enableStatic;
        urbit-king.components.exes.urbit-king.configureFlags =
          lib.optionals enableStatic [
            "--ghc-option=-optl=-L${gmp}/lib"
            "--ghc-option=-optl=-L${libffi}/lib"
            "--ghc-option=-optl=-L${zlib}/lib"
          ] ++ lib.optionals (enableStatic && stdenv.isDarwin)
          [ "--ghc-option=-optl=-L${darwin.libiconv}/lib" ];
      };
    }];
  };

in project // {
  # Build and obtain an executable from Hackage, using the same compiler
  # and cabal index-state as the stackProject.
  #
  # This allows specifying the executable name, in contrast with shellFor.tools.
  hackageTool = { name, version, exe ? lib.toLower name }:
    (haskell-nix.hackage-package {
      inherit compiler-nix-name index-state name version;
    }).components.exes.${exe};
}
