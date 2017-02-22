{ crossenv }:
let pkgs =
rec {
  inherit (crossenv) binutils gcc;

  hello = import ./pkgs/hello {
    inherit crossenv;
  };

  hello_cpp = import ./pkgs/hello_cpp {
    inherit crossenv;
  };

  pdcurses = import ./pkgs/pdcurses {
    inherit crossenv;
  };

  readline = import ./pkgs/readline {
    inherit crossenv pdcurses;
  };

  expat = import ./pkgs/expat {
    inherit crossenv;
  };

  zlib = import ./pkgs/zlib {
    inherit crossenv;
  };

  gdb = import ./pkgs/gdb {
    inherit crossenv zlib expat readline pdcurses;
  };

  libusbp = import ./pkgs/libusbp {
    inherit crossenv;
  };

  p-load = import ./pkgs/p-load {
    inherit crossenv libusbp;
  };

  angle = import ./pkgs/angle {
    inherit crossenv;
    debug = false;
  };

  angle_util = import ./pkgs/angle/util.nix {
    inherit crossenv angle;
  };

  angle_samples = import ./pkgs/angle/samples.nix {
    inherit crossenv angle angle_util;
  };

  qt58 = import ./pkgs/qt58 {
    inherit crossenv angle;
  };
};
in pkgs
