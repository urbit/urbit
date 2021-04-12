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

  pkgsLocal = import ./default.nix { };

  # The non-Haskell packages which build inputs (dependencies) will be
  # propagated into the shell. This combines nixpkgs' mkShell behaviour
  # with Haskell.nix's shellFor.
  #
  # For example, adding urbit here results in gmp, h2o, zlib, etc. being
  # made available, so you can just run make.
  # 
  # Typically the inputs listed here also have a shell.nix in their respective
  # source directory you can use, to avoid the Haskell/GHC dependencies.
  inputsFrom = with pkgsLocal; [ ent ge-additions herb libaes_siv urbit ];

  # Collect the named attribute from all dependencies listed in inputsFrom.
  mergeFrom = name: pkgs.lib.concatLists (pkgs.lib.catAttrs name inputsFrom);

in pkgsLocal.hs.shellFor {
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

  # Haskell tools to make available on the shell's PATH.
  tools = {
    shellcheck = "0.7.1";
    ormolu = "0.1.3.0";
  };

  # Nixpkgs tools to make available on the shell's PATH.
  buildInputs = [
    pkgs.cacert
    pkgs.nixfmt
    pkgs.shfmt
    pkgs.stack
    (import pkgs.sources.niv { }).niv
  ] ++ mergeFrom "buildInputs";

  nativeBuildInputs = mergeFrom "nativeBuildInputs";
  propagatedBuildInputs = mergeFrom "propagatedBuildInputs";
  propagatedNativeBuildInputs = mergeFrom "propagatedNativeBuildInputs";

  shellHook = pkgs.lib.concatStringsSep "\n"
    (pkgs.lib.catAttrs "shellHook" (pkgs.lib.reverseList inputsFrom));
}
