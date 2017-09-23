{ native }:
let
  nixpkgs = native.nixpkgs;

  arch = "x86_64";

  darwin_name = "darwin15";

  host = "${arch}-apple-${darwin_name}";

  os = "macos";

  compiler = "clang";

  exe_suffix = "";

  osxcross = ./osxcross;

  clang = native.make_derivation rec {
    name = "clang";

    version = "5.0.0";

    src = nixpkgs.fetchurl {
      url = "https://llvm.org/releases/${version}/cfe-${version}.src.tar.xz";
      sha256 = "0w09s8fn3lkn6i04nj0cisgp821r815fk5b5fjn97xrd371277q1";
    };

    llvm_src = nixpkgs.fetchurl {
      url = "https://llvm.org/releases/${version}/llvm-${version}.src.tar.xz";
      sha256 = "1nin64vz21hyng6jr19knxipvggaqlkl2l9jpd5czbc4c2pcnpg3";
    };

    patches = [ ./clang_megapatch.patch ];

    builder = ./clang_builder.sh;

    native_inputs = [ nixpkgs.python2 ];

    cmake_flags =
      "-DCMAKE_BUILD_TYPE=Release " +
      # "-DCMAKE_BUILD_TYPE=Debug " +
      "-DLLVM_ENABLE_ASSERTIONS=OFF";
  };

  # Note: We use nixpkgs.clang so we can compile an objective C library (which
  # probably isn't needed).  We can't use our own clang because it doesn't
  # quite work yet for compiling native executables.  Would be nice to get
  # rid of the dependency on nixpkgs.clang though and just build everything
  # with the normal nixpkgs GCC toolchain.
  #
  # Note: cctools shows a warning about llvm-config not found so disabling LTO
  # support.
  #
  # Note: We need TAPI (.tbd file) support so we can link against libraries from
  # the Mac OS X SDK, so we use commit 8e9c3f2.  The commit after that (22ebe72
  # on 2017-04-01) added TAPIv2 support, which adds a new external dependency on
  # libtapi, which is meant to be built as part of llvm, which will probably
  # increase the complexity of this build a lot.  As of 2017-09-16, there have
  # been no commits after that one, and the osxcross project is still on commit
  # 8e9c3f2, so this is the more-traveled route.
  cctools = native.make_derivation {
    name = "cctools";
    builder = ./cctools_builder.sh;
    src = nixpkgs.fetchurl {
      url = "https://github.com/tpoechtrager/cctools-port/archive/8e9c3f2.tar.gz";
      sha256 = "04p5b1ix52yk48f09xkdv11ki8cc1zwzvm0dk2j8ylb8jk1a04y4";
    };
    configure_flags = "--target=${host}";
    native_inputs = [
      nixpkgs.clang
      nixpkgs.libtool
      nixpkgs.autoconf
      nixpkgs.automake
      nixpkgs.m4
    ];
  };

  xar_src = nixpkgs.fetchurl {
    url = "https://github.com/downloads/mackyle/xar/xar-1.6.1.tar.gz";
    sha256 = "0ghmsbs6xwg1092v7pjcibmk5wkyifwxw6ygp08gfz25d2chhipf";
  };

  xar = native.make_derivation {
    name = "xar";
    builder = ./xar_builder.sh;
    src = xar_src;
    native_inputs = [
      nixpkgs.libxml2.dev
      nixpkgs.openssl.dev
      nixpkgs.zlib.dev
      nixpkgs.pkgconfig
    ];
  };

  sdk = native.make_derivation rec {
    name = "macos-sdk";
    builder = ./sdk_builder.sh;
    version = "10.11";
    src = ./MacOSX10.11.sdk.tar.xz;
  };

  macos_version_min = "10.11";

  toolchain = native.make_derivation rec {
    name = "mac-toolchain";
    builder = ./builder.sh;
    inherit host osxcross sdk;
    native_inputs = [ clang cctools xar ];

    CXXFLAGS =
      "-std=c++11 " +
      "-Wall " +
      "-I. " +
      "-O2 -g " +
      "-DOSXCROSS_VERSION=\\\"0.15\\\" " +
      "-DOSXCROSS_TARGET=\\\"${darwin_name}\\\" " +
      "-DWRAPPER_OS_VERSION_MIN=\\\"${macos_version_min}\\\" " +
      "-DWRAPPER_SDK_PATH=\\\"/nix/store/shs3mnp6j07sv2xzzs92a4ydbvb6fs0w-macos-sdk\\\" " +
      "-DWRAPPER_SDK_VERSION=\\\"10.11\\\" " +
      "-DWRAPPER_HOST=\\\"${host}\\\" " +
      "-DWRAPPER_PATH=\\\"${cctools}/bin:${clang}/bin\\\"";
  };

  cmake_toolchain = import ../cmake_toolchain {
    cmake_system_name = "Darwin";
    inherit nixpkgs host;
  };

  crossenv = {
    # Target info variables.
    inherit host arch os compiler exe_suffix;

    # Cross-compiling toolchain.
    inherit toolchain;
    toolchain_drvs = [ toolchain clang cctools xar ];

    # Build tools and variables to support them.
    inherit cmake_toolchain;

    # nixpkgs: a wide variety of programs and build tools.
    inherit nixpkgs;

    # Some native build tools made by nixcrpkgs.
    inherit native;

    inherit clang cctools xar sdk;

    make_derivation = import ../make_derivation.nix nixpkgs crossenv;
  };
in
  crossenv
