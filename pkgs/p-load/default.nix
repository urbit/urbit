{ crossenv, libusbp }:

crossenv.nixpkgs.stdenv.mkDerivation rec {
  name = "p-load-${version}-${crossenv.host}";

  version = "2.0.1ish";

  host = crossenv.host;
  cmake_toolchain = crossenv.cmake_toolchain;

  src = crossenv.nixpkgs.fetchurl {
    url = "https://github.com/pololu/p-load/archive/31126da.tar.gz";
    sha256 = "1rha46s1jq6060c9mkki3rra9s4hsg2vz4cakcq91d98zb6i63ln";
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
