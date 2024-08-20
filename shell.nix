# A repository wide shell.nix containing all tools, formatters, and inputs
# required to build any of the C packages.
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

  # The packages from which build inputs (dependencies) will be propagated into
  # the shell.
  #
  # For example, adding urbit here results in gmp, h2o, zlib, etc. being
  # made available, so you can just run make.
  #
  # Typically the inputs listed here also have a shell.nix in their respective
  # source directory you can use directly.
  inputsFrom = with pkgsLocal; [ ent urbit urcrypt ];

  # Collect the named attribute from all dependencies listed in inputsFrom.
  mergeFrom = name: pkgs.lib.concatLists (pkgs.lib.catAttrs name inputsFrom);

in pkgs.mkShell {
  # Nixpkgs tools to make available on the shell's PATH.
  buildInputs = [
    pkgs.cacert
    pkgs.nixpkgs-fmt
    pkgs.shfmt
    (import pkgs.sources.niv { }).niv
  ] ++ mergeFrom "buildInputs";

  nativeBuildInputs = mergeFrom "nativeBuildInputs";
  propagatedBuildInputs = mergeFrom "propagatedBuildInputs";
  propagatedNativeBuildInputs = mergeFrom "propagatedNativeBuildInputs";

  shellHook = pkgs.lib.concatStringsSep "\n"
    (pkgs.lib.catAttrs "shellHook" (pkgs.lib.reverseList inputsFrom));
}
