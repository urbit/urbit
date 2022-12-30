# The flake.nix is the entrypoint of all nix code.
#
# This repository uses the standard tool https://github.com/divnix/std.
# Familiarity with std is required to be able to contribute effectively.
# While official documentation for std can be found in its GitHub, this flake
# has been thoroughly commented so as to quickstart new maintainers.
# This flake can also be used as a template for new std-based projects.
# Further documentation can be found in nix/README.md
#
# You may want to refer to the standard glossary as you go along:
# https://divnix.github.io/std/glossary.html
{
  description = "Urbit - An operating function";

  inputs = {
    nixpkgs = {
      url = "github:NixOS/nixpkgs/376d67e1cd05d5ac8a64a3f47f17b80fb6394792";
    };
    std = {
      url = "github:divnix/std";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  # The flake outputs are managed by std.
  outputs = inputs:

    # The growOn function takes care of producing the flake outputs.
    inputs.std.growOn
      {

        # Boilerplate
        inherit inputs;

        # All nix files will reside inside this folder, no exception.
        # Each subfolder of cellsFrom is a "cell".
        # Cell names are arbitrary; a cell name is its folder name.
        # Cells are for highest-level organization and grouping of nix code.
        cellsFrom = ./nix/cells;

        # Each cell contains "cell blocks".
        # Block names are arbitrary.
        # Each block can be thought of as providing a "feature" to its cell.
        # Cell blocks have types.
        # Each cell block must be either:
        #   A nix file named after the cell block
        #   A directory named after the cell block and containing a default.nix
        # Not all cells have the same cell blocks.
        # All cell blocks belong in a cell.
        #
        # In this repository we have three cell blocks, listed below with their type:
        #   devshells :: devshells
        #     Development shells available via nix develop
        #   packages :: installables
        #     Derivations available via nix build
        #   libraries :: functions
        #     Everything that is not a derivation goes here
        #     Includes functions, attrsets and simple literal values shared across cells
        #     These are not exposed to the flake

        # std provides a TUI to interact with the cell blocks.
        # Available interactions are determined by the cell block's type.
        # Because this repository does not yet use the TUI, the type is mostly irrelevant.
        cellBlocks = [
          (inputs.std.devshells "devshells")
          (inputs.std.installables "packages")
          (inputs.std.functions "libraries")
        ];
      }

      # The growOn function will then accept an arbitrary number of "soil" attrs.
      # This is where we translate cells and cell blocks into a standard nix flake
      # outputs attrs.
      #
      # This is where we also decide which cells and which cell blocks will
      # make it into the flake. To exclude stuff from the flake, we simply
      # do not "harvest" it.
      #
      # The attrs will be recursively merged in the order in which they appear.
      {
        # Here we say that we want the "devshells" cell block of the plutus cell
        # (which contains a number of shell-able derivations) to be exposed
        # by the flake and accessible via nix develop.
        devShells = inputs.std.harvest inputs.self [ "urbit" "devshells" ];

        # Here we say that we want the "packages" cell block of the plutus cell
        # (which contains a number of buildable derivations) to be exposed
        # by the flake and accessible via nix build (or nix run).
        packages = inputs.std.harvest inputs.self [ "urbit" "packages" ];
      };
}
