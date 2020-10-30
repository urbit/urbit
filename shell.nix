# A repository wide shell.nix containing all tools, formatters, and inputs
# required to build any of the C or Haskell packages.
#
# Entering a nix-shell using this derivation will allow you to cd anywhere
# in the ./pkg directory and run the appropriate build tooling.
#
# See the individual ./pkg/* directories for shell.nix derivations that only
# propagate minimal sets of buildInputs for the related package.
# (import ./default.nix { }).shell

let

  pkgs = import ./nix/default.nix { };
  localPackages = import ./default.nix { };

  # The non-Haskell packages which build inputs (dependencies) will be
  # propagated into the shell. This combines nixpkgs' mkShell behaviour
  # with Haskell.nix's shellFor.
  #
  # For example, adding urbit here results in gmp, h2o, zlib, etc. being
  # made available, so you can just run make.
  # 
  # Typically the inputs listed here also have a shell.nix in their respective
  # source directory you can use, to avoid the Haskell/GHC dependencies.
  inputsFrom = with localPackages; [ ent ge-additions herb libaes_siv urbit ];

  merge = name: pkgs.lib.concatLists (pkgs.lib.catAttrs name inputsFrom);

in localPackages.hs.shellFor {
  # Haskell packages from the stackProject which will have their
  # dependencies available in the shell.
  packages = ps:
    with ps; [
      lmdb-static
      racquire
      terminal-progress-bar
      urbit-atom
      urbit-azimuth
      urbit-eventlog-lmdb
      urbit-king
      urbit-noun
      urbit-noun-core
      urbit-termsize
    ];

  # Build tools to make available in the shell's PATH.
  buildInputs = [
    pkgs.cacert
    pkgs.stack
    pkgs.nixfmt
    pkgs.shfmt

    (import pkgs.sources.niv { }).niv

    (localPackages.hs.hackageTool {
      name = "ormolu";
      version = "0.1.3.0";
    })

    (localPackages.hs.hackageTool {
      name = "ShellCheck";
      version = "0.7.1";
    })
  ] ++ merge "buildInputs";

  nativeBuildInputs = merge "nativeBuildInputs";
  propagatedBuildInputs = merge "propagatedBuildInputs";
  propagatedNativeBuildInputs = merge "propagatedNativeBuildInputs";

  shellHook = pkgs.lib.concatStringsSep "\n"
    (pkgs.lib.catAttrs "shellHook" (pkgs.lib.reverseList inputsFrom));
}
