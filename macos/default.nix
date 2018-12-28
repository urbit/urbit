# Note: To reduce clutter here, it might be nice to move clang to
# `native`, and also make `native` provide a function for building
# binutils.  So clang and binutils recipes could be shared by the
# different platforms we targets.

{ native }:
let
  nixpkgs = native.nixpkgs;

  arch = "x86_64";

  # was darwin15, changed to darwin so that lld guesses flavor=Darwin correctly
  darwin_name = "darwin15";

  macos_version_min = "10.11";

  host = "${arch}-apple-${darwin_name}";

  os = "macos";

  compiler = "clang";

  exe_suffix = "";

  clang = native.make_derivation rec {
    name = "clang";

    version = "7.0.1";

    src = nixpkgs.fetchurl {
      url = "http://releases.llvm.org/${version}/cfe-${version}.src.tar.xz";
      sha256 = "067lwggnbg0w1dfrps790r5l6k8n5zwhlsw7zb6zvmfpwpfn4nx4";
    };

    llvm_src = nixpkgs.fetchurl {
      url = "http://releases.llvm.org/${version}/llvm-${version}.src.tar.xz";
      sha256 = "16s196wqzdw4pmri15hadzqgdi926zln3an2viwyq0kini6zr3d3";
    };

    # Note: We aren't actually using lld for anything yet.
    lld_src = nixpkgs.fetchurl {
      url = "http://releases.llvm.org/${version}/lld-${version}.src.tar.xz";
      sha256 = "0ca0qygrk87lhjk6cpv1wbmdfnficqqjsda3k7b013idvnralsc8";
    };

    patches = [ ./clang_megapatch.patch ];

    builder = ./clang_builder.sh;

    native_inputs = [ nixpkgs.python2 ];

    cmake_flags =
      "-DCMAKE_BUILD_TYPE=Release " +
      # "-DCMAKE_BUILD_TYPE=Debug " +
      "-DLLVM_TARGETS_TO_BUILD=X86\;ARM " +
      # ld64 uses dynamic_cast, requiring rtti
      # Can probably remove this option now that ld64 doesn't depend on clang.
      "-DLLVM_ENABLE_RTTI=ON " +
      "-DLLVM_ENABLE_ASSERTIONS=OFF";
  };

  # Previously we used a port of Apple's TAPI library from here:
  #   https://github.com/DavidEGrayson/tapi/archive/f98d0c3.tar.gz
  # Another version of that libtrary is here:
  #   https://github.com/tpoechtrager/apple-libtapi
  # But those versions require LLVM and clang 5.0.0, so now we are using
  # tinytapi instead.
  tapi = native.make_derivation rec {
    name = "tinytapi";
    # tmphax until tinytapi is published on GitHub
    src_dir = ../../tinytapi/src;
    include_dir = ../../tinytapi/include;
    builder = ./tinytapi_builder.sh;
    libyaml = nixpkgs.libyaml;
    native_inputs = [ libyaml ];
  };

  cctools_commit = "c1cc758";
  cctools_apple_version = "274.2";  # from README.md
  cctools_port_src = nixpkgs.fetchurl {
    url = "https://github.com/tpoechtrager/cctools-port/archive/${cctools_commit}.tar.gz";
    sha256= "11bfcndzbdmjp2piabyqs34da617fh5fhirqvb9w87anfan15ffa";
  };

  ld = native.make_derivation rec {
    name = "cctools-ld64";
    apple_version = cctools_apple_version;
    src = cctools_port_src;
    patches = [
      ./cctools-format.patch
      ./cctools-ld64-registers.patch
    ];
    builder = ./ld_builder.sh;
    native_inputs = [ tapi ];
    inherit host;
  };

  ranlib = native.make_derivation rec {
    name = "cctools-ranlib";
    apple_version = cctools_apple_version;
    src = ld.src;
    builder = ./ranlib_builder.sh;
    patches = [
      ./cctools-format.patch
      ./cctools-bytesex.patch
    ];
    inherit host;
  };

  ar = native.make_derivation rec {
    name = "cctools-ar";
    apple_version = cctools_apple_version;
    src = cctools_port_src;
    builder = ./ar_builder.sh;
    patches = [
      ./cctools-format.patch
      ./cctools-libstuff-no-error.patch
    ];
    inherit host ranlib;
  };

  strip = native.make_derivation rec {
    name = "cctools-strip";
    apple_version = cctools_apple_version;
    src = cctools_port_src;
    builder = ./strip_builder.sh;
    patches = [
      ./cctools-format.patch
    ];
    inherit host;
  };

  # TODO: add instructions for building the SDK tarball, probably want a copy of
  # the script from osxcross.
  sdk = native.make_derivation rec {
    name = "macos-sdk";
    builder = ./sdk_builder.sh;
    src = ./MacOSX.sdk.tar.xz;
  };

  toolchain = native.make_derivation rec {
    name = "macos-toolchain";
    builder = ./toolchain_builder.sh;
    src_file = ./wrapper.cpp;
    inherit host clang ld ranlib ar strip sdk;

    CXXFLAGS =
      "-std=c++11 " +
      "-Wall " +
      "-I. " +
      "-O2 -g " +
      "-DWRAPPER_OS_VERSION_MIN=\\\"${macos_version_min}\\\" " +
      "-DWRAPPER_HOST=\\\"${host}\\\" " +
      "-DWRAPPER_ARCH=\\\"${arch}\\\" " +
      "-DWRAPPER_SDK_PATH=\\\"${sdk}\\\" " +
      "-DWRAPPER_LINKER_VERSION=\\\"${ld.apple_version}\\\"";
  };

  cmake_toolchain = import ../cmake_toolchain {
    cmake_system_name = "Darwin";
    inherit nixpkgs host;
  };

  crossenv = {
    is_cross = true;

    # Build tools available on the PATH for every derivation.
    default_native_inputs = native.default_native_inputs
      ++ [ clang toolchain native.wrappers ];

    # Target info environment variables.
    inherit host arch os compiler exe_suffix macos_version_min;

    # CMake toolchain file.
    inherit cmake_toolchain;

    # A wide variety of programs and build tools.
    inherit nixpkgs;

    # Some native build tools made by nixcrpkgs.
    inherit native;

    # License information that should be shipped with any software
    # compiled by this environment.
    global_license_set = { };

    # Make it easy to build or refer to the build tools.
    inherit clang tapi ld ranlib ar sdk toolchain;

    make_derivation = import ../make_derivation.nix crossenv;
  };
in
  crossenv
