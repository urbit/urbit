{ crossenv, libusbp }:

crossenv.nixpkgs.stdenv.mkDerivation rec {
  name = "p-load-${version}-${crossenv.host}";

  version = "2.0.1";

  host = crossenv.host;
  cmake_toolchain = crossenv.cmake_toolchain;

  src = crossenv.nixpkgs.fetchurl {
    url = "https://github.com/pololu/p-load/archive/${version}.tar.gz";
    sha256 = "0gj55nap8xm3q97hkllzi6ni8razx1xh98bivfzlp5ipxf0w1qxp";
  };

  buildInputs = [
    crossenv.gcc
    crossenv.binutils
    crossenv.nixpkgs.cmake
    crossenv.nixpkgs.pkgconfig
    libusbp
  ];

  builder = ./builder.sh;
}
