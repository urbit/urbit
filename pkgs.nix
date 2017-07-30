{ crossenv }:
let pkgs =
rec {
  recurseForDerivations = true;
  inherit (crossenv) binutils gcc;

  hello = import ./pkgs/hello {
    inherit crossenv;
  };

  hello_cpp = import ./pkgs/hello_cpp {
    inherit crossenv;
  };

  usbview = import ./pkgs/usbview {
    inherit crossenv;
  };

  devcon = import ./pkgs/devcon {
    inherit crossenv;
  };

  pdcurses = import ./pkgs/pdcurses {
    inherit crossenv;
  };

  readline = import ./pkgs/readline {
    inherit crossenv;
    curses = pdcurses;
  };

  expat = import ./pkgs/expat {
    inherit crossenv;
  };

  zlib = import ./pkgs/zlib {
    inherit crossenv;
  };

  gdb = import ./pkgs/gdb {
    inherit crossenv expat;
    curses = pdcurses;
  };

  libudev = import ./pkgs/libudev {
    inherit crossenv;
  };

  libusbp = import ./pkgs/libusbp {
    inherit crossenv libudev;
  };

  p-load = import ./pkgs/p-load {
    inherit crossenv libusbp;
  };

  angle = import ./pkgs/angle {
    inherit crossenv gdb;
  };

  xcb-proto = import ./pkgs/xcb-proto {
    inherit crossenv;
  };

  xorg-macros = import ./pkgs/xorg-macros {
    inherit crossenv;
  };

  xproto = import ./pkgs/xproto {
    inherit crossenv xorg-macros;
  };

  libxau = import ./pkgs/libxau {
    inherit crossenv xorg-macros xproto;
  };

  libxcb = import ./pkgs/libxcb {
    inherit crossenv xcb-proto xorg-macros libxau;
  };

  qt = import ./pkgs/qt58 {
    inherit crossenv libudev libxcb;
  };

  pavr2 = import ./pkgs/pavr2 {
    inherit crossenv libusbp;
    qt = qt.base;
  };
};
in pkgs
