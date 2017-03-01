{ crossenv }:

let

  pdcurses = import ./lib.nix {
    inherit crossenv;
  };

  examples = import ./examples.nix {
    inherit crossenv pdcurses;
  };

in
  pdcurses // { inherit examples; }
