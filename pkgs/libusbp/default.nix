{ crossenv }:

# TODO: get rid of boilerplate, refer to nixpkgs less (for fetchurl it is fine)
crossenv.nixpkgs.stdenv.mkDerivation rec {
  name = "libusbp-${version}-${crossenv.host}";

  version = "1.0.1ish";

  host = crossenv.host;
  cmake_toolchain = crossenv.cmake_toolchain;

  src = crossenv.nixpkgs.fetchurl {
    url = "https://github.com/pololu/libusbp/archive/245f5e4.tar.gz";
    sha256 = "1xnv5w1c9zcdsdmgacgb1y5qlwpj0868v3cwcmbxxdx89hcsw2mp";
  };

  patches = [
  ];

  buildInputs = [
    crossenv.gcc
    crossenv.binutils
    crossenv.nixpkgs.cmake
  ];

  builder = ./builder.sh;
}
