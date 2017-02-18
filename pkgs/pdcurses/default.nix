{ crossenv }:

let

  pdcurses = import ./lib.nix {
    inherit crossenv;
  };

  demos = import ./demos.nix {
    inherit crossenv pdcurses;
  };

in
  pdcurses // { inherit demos; }

# TODO: fix the issues revealed by the testcurs demo in mingw-w64
