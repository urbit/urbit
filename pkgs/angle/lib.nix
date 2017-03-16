{ crossenv, debug ? false }:

let
  target = if crossenv.os == "windows" then "win32"
           else throw "unknown target";
in
crossenv.nixpkgs.stdenv.mkDerivation rec {
  name = "angle-${version}-${crossenv.host}";

  version = "2017-03-09";

  src = crossenv.nixpkgs.fetchgit {
    url = "https://chromium.googlesource.com/angle/angle";
    rev = "fe9306a8e5bb6a8d52368e8e7b8e92f3bc7e77d4";
    sha256 = "0m2pbkl9x9kybcxzhai0s3bk9k0r8nb531gzlxcvb3gb2za388bn";
  };

  patches = [ ./megapatch.patch ];

  builder = ./builder.sh;

  buildInputs = [
    crossenv.gcc
    crossenv.binutils
    crossenv.nixpkgs.pythonPackages.gyp
    crossenv.nixpkgs.ninja
  ];

  GYP_GENERATORS = "ninja";

  gypFlags =
      "-D OS=${crossenv.gyp_os} " +
      "-D TARGET=${target} " +
      "-D use_ozone=0 " +
      "-D angle_enable_vulkan=0 " +   # Vulkan support is in progress
      "-D angle_gl_library_type=static_library " +
      "-I ../src/gyp/common.gypi " +
      "--depth .";

  CC_target = "${crossenv.host}-gcc";
  CXX_target = "${crossenv.host}-g++";
  AR = "${crossenv.host}-ar";
  RANLIB = "${crossenv.host}-ranlib";

  CXXFLAGS = "-msse2 -Wno-conversion-null";

  inherit debug;
}
