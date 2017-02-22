{ crossenv, debug ? false }:

crossenv.nixpkgs.stdenv.mkDerivation rec {
  name = "angle-${version}-${crossenv.host}";

  version = "2017-02-17";

  src = crossenv.nixpkgs.fetchgit {
    url = "https://chromium.googlesource.com/angle/angle";
    rev = "4e0e6f8acf8b56898add4facb3af6ffa409cc1d0";
    sha256 = "0nib2zr6jl2vwalpc00igh5yv53p1rb5gh03mfdljf1ym6w2vhjb";
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
      "-D TARGET=win32 " +  # TODO
      "-D use_ozone=0 " +
      "-D angle_enable_vulkan=0 " +   # Vulkan support is in progress
      "-D angle_gl_library_type=static_library " +
      "-I ../src/gyp/common.gypi " +
      "--depth .";

  CC_target = "${crossenv.host}-gcc";
  CXX_target = "${crossenv.host}-g++";
  AR = "${crossenv.host}-ar";
  RANLIB = "${crossenv.host}-ranlib";

  CXXFLAGS = "-msse2 -Wno-conversion-null -DMINGW_HAS_SECURE_API";

  inherit debug;
}
