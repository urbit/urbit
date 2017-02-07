{ crossenv }:

crossenv.nixpkgs.stdenv.mkDerivation rec {
  name = "angle-${version}-${crossenv.host}";

  version = "2017-02-07";

  src = crossenv.nixpkgs.fetchgit {
    url = "https://chromium.googlesource.com/angle/angle";
    rev = "1d37bc508694e75f087261aafb31f232e7d62ddd";
    sha256 = "1vahwf33h2wspp2cr109b2ajvh8yv2cj40yb61y10mqz7v3h0x63";
  };

  builder = ./builder.sh;

  buildInputs = [
    crossenv.gcc
    crossenv.binutils
    crossenv.nixpkgs.pythonPackages.gyp
    crossenv.nixpkgs.ninja
  ];

  # MSYS2 options: -D MSVS_VERSION="" -D TARGET=${_target} --format make --depth . 
  gypFlags =
      "-D OS=${crossenv.os} " +
      "-D TARGET=win32 " +  # TODO
      "-D use_ozone=0 " +
      "-I ../src/gyp/common.gypi " +
      "--depth .";

  CC_target = "${crossenv.host}-gcc";
  CXX_target = "${crossenv.host}-g++";

  GYP_GENERATORS = "ninja";
}
