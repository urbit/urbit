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

# TODO: fix the issues revealed by the testcurs demo in mingw-w64
