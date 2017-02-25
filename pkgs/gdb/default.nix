{ crossenv, zlib, expat, readline, pdcurses }:

# TODO: provide a mingw-w64 isl to gdb because its configure script looks for it?

crossenv.nixpkgs.stdenv.mkDerivation rec {
  name = "gdb-${version}-${crossenv.host}";

  version = "7.12.1";

  src = crossenv.nixpkgs.fetchurl {
    url = "http://ftp.gnu.org/gnu/gdb/gdb-${version}.tar.xz";
    sha256 = "11ii260h1sd7v0bs3cz6d5l8gqxxgldry0md60ncjgixjw5nh1s6";
  };

  patches = [
    ./substitute-path-all-filenames.patch  # TODO: consider removing if upstream does not accept
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

  inherit zlib expat readline pdcurses;

  builder = ./builder.sh;

  inherit (crossenv) host;
}
