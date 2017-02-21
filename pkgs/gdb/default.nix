{ crossenv, zlib, expat, readline }:

# TODO: provide a mingw-w64 isl to gdb because its configure script looks for it?

crossenv.nixpkgs.stdenv.mkDerivation rec {
  name = "gdb-${version}-${crossenv.host}";

  version = "7.12";

  src = crossenv.nixpkgs.fetchurl {
    url = "http://ftp.gnu.org/gnu/gdb/gdb-${version}.tar.xz";
    sha256 = "152g2qa8337cxif3lkvabjcxfd9jphfb2mza8f1p2c4bjk2z6kw3";
  };

  buildInputs = [
    crossenv.gcc
    crossenv.binutils
    crossenv.nixpkgs.texinfo
    crossenv.nixpkgs.bison
    crossenv.nixpkgs.yacc
    crossenv.nixpkgs.m4
    crossenv.nixpkgs.flex
  ];

  inherit zlib expat readline;

  builder = ./builder.sh;

  inherit (crossenv) host;
}
