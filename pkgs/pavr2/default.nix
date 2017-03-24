{ crossenv, qt, libusbp }:

crossenv.nixpkgs.stdenv.mkDerivation rec {
  name = "pavr2-${version}-${crossenv.host}";

  version = "1.0.2";

  src = crossenv.nixpkgs.fetchurl {
    url = "https://github.com/pololu/pololu-usb-avr-programmer-v2/archive/${version}.tar.gz";
    sha256 = "0xy36gj652ziwzdj7r6zcsgr86wdc742j2k3qy9qkfayvw23bg9q";
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
