{ crossenv, qt, libusbp }:

crossenv.nixpkgs.stdenv.mkDerivation rec {
  name = "pavr2-${version}-${crossenv.host}";

  version = "1.0.2ish";

  src = crossenv.nixpkgs.fetchurl {
    url = "https://github.com/pololu/pololu-usb-avr-programmer-v2/archive/053fb01.tar.gz";
    sha256 = "0mh3qyvk4k5i82zcf4q0anvm8aq1b684v91ss2kqhz80fafm6gy3";
  };

  builder = ./builder.sh;

  buildInputs = [
    crossenv.binutils
    crossenv.gcc
    crossenv.pkg-config
    crossenv.pkg-config-cross
    crossenv.nixpkgs.cmake
  ];

  inherit (crossenv) cmake_toolchain;

  PKG_CONFIG = "${crossenv.host}-pkg-config";
  I686_W64_MINGW32_PKG_CONFIG_PATH = "${libusbp}/lib/pkgconfig"; # TODO

  CMAKE_PREFIX_PATH="";
}
