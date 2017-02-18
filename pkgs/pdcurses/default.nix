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
