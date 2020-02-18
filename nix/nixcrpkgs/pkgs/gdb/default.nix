# Note: This package has only been tested on Windows, and the pdcurses library
# it uses does not support Linux in console mode or mac OS X.

# Note: GDB has a bundled copy of readline that it uses.
# There is a --with-system-readline option we could try to use.

# Note: consider providing a mingw-w64 isl to gdb because its configure script looks for it

{ crossenv, expat, curses }:

crossenv.make_derivation rec {
  name = "gdb-${version}";

  version = "8.2.1";

  src = crossenv.nixpkgs.fetchurl {
    url = "https://ftp.gnu.org/gnu/gdb/gdb-${version}.tar.xz";
    sha256 = "00i27xqawjv282a07i73lp1l02n0a3ywzhykma75qg500wll6sha";
  };

  patches = [];

  native_inputs = [
    crossenv.nixpkgs.texinfo
    crossenv.nixpkgs.bison
    crossenv.nixpkgs.yacc
    crossenv.nixpkgs.m4
    crossenv.nixpkgs.flex
  ];

  inherit expat curses;

  builder = ./builder.sh;
}
