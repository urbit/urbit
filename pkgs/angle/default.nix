{ crossenv }:

crossenv.nixpkgs.stdenv.mkDerivation rec {
  name = "angle-${version}-${crossenv.host}";

  version = "2017-02-07";

  src = crossenv.nixpkgs.fetchgit {
    url = "https://chromium.googlesource.com/angle/angle";
    rev = "1d37bc508694e75f087261aafb31f232e7d62ddd";
    sha256 = "1vahwf33h2wspp2cr109b2ajvh8yv2cj40yb61y10mqz7v3h0x63";
  };

  patches = [
    ./megapatch.patch
  ];

  builder = ./builder.sh;

  buildInputs = [
    crossenv.gcc
    crossenv.binutils
    crossenv.nixpkgs.pythonPackages.gyp
    crossenv.nixpkgs.ninja
  ];

  # MSYS2 options: -D MSVS_VERSION="" -D TARGET=${_target} --format make --depth .
  gypFlags =
      "-D OS=${crossenv.gyp_os} " +
      "-D TARGET=win32 " +  # TODO
      "-D use_ozone=0 " +
      "-I ../asrc/gyp/common.gypi " +
      "--depth .";

  CC_target = "${crossenv.host}-gcc";
  CXX_target = "${crossenv.host}-g++";

  GYP_GENERATORS = "ninja";

  patchTmphax = "cp ${../../angle-src/src/common/string_utils.cpp} src/common/string_utils.cpp; " +
                "cp ${../../angle-src/src/angle.gyp} src/angle.gyp; " +
                "cp ${../../angle-src/gyp/common_defines.gypi} gyp/common_defines.gypi; ";
}
