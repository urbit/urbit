{ crossenv, zlib, expat, curses }:

# Note: GDB has a bundled copy of readline that it uses.
# There is a --with-system-readline option we could try to use.

# TODO: provide a mingw-w64 isl to gdb because its configure script looks for it?

crossenv.nixpkgs.stdenv.mkDerivation rec {
  name = "gdb-${version}-${crossenv.host}";

  version = "7.12.1";

  src = crossenv.nixpkgs.fetchurl {
    url = "http://ftp.gnu.org/gnu/gdb/gdb-${version}.tar.xz";
    sha256 = "11ii260h1sd7v0bs3cz6d5l8gqxxgldry0md60ncjgixjw5nh1s6";
  };

  patches = [
    # Make GCC better at finding source files.
    # https://sourceware.org/ml/gdb-patches/2017-02/msg00693.html
    ./substitute-path-all-filenames.patch
  ];

  buildInputs = [
    crossenv.gcc
    crossenv.binutils
    crossenv.nixpkgs.texinfo
    crossenv.nixpkgs.bison
    crossenv.nixpkgs.yacc
    crossenv.nixpkgs.m4
    crossenv.nixpkgs.flex
  ];

  inherit zlib expat curses;

  builder = ./builder.sh;

  inherit (crossenv) host;
}
