{ native }:
let
  nixpkgs = native.nixpkgs;

  arch = "x86_64";

  darwin_name = "darwin15";

  host = "${arch}-apple-${darwin_name}";

  os = "macos";

  compiler = "clang";

  exe_suffix = "";

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
      "-DLLVM_TARGETS_TO_BUILD=X86\;ARM " +
      "-DLLVM_ENABLE_ASSERTIONS=OFF";
  };

  tapi = native.make_derivation rec {
    name = "tapi";
    version = "2.0.0";
    src = nixpkgs.fetchurl {
      url = "https://github.com/DavidEGrayson/tapi/archive/50cfa2b.tar.gz";
      sha256 = "17356qy4jmpg92cb74gcccfh3prq3mvkj53yqn07krk7v1snjvc7";
    };
    builder = ./tapi_builder.sh;
    native_inputs = [ clang ];
    inherit clang;
  };

  # TODO: add instructions for building the SDK tarball, probably want a copy of
  # the script from osxcross.
  sdk = native.make_derivation rec {
    name = "macos-sdk";
    builder = ./sdk_builder.sh;
    version = "10.11";
    src = ./MacOSX10.11.sdk.tar.xz;
  };

  # A trimmed-down SDK without harmful headers like locale.h, which conflict
  # with glibc's locale.h.
  sdk_lite = native.make_derivation rec {
    name = "macos-sdk-lite";
    builder = ./sdk_lite_builder.sh;
    inherit sdk;
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
  cctools_tpoechtrager = native.make_derivation {
    name = "cctools_tpoechtrager-${host}";
    builder = ./cctools_tpoechtrager_builder.sh;
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

  cctools = native.make_derivation rec {
    name = "cctools-${host}";
    builder = ./cctools_builder.sh;
    version = "895";
    inherit host;
    src = nixpkgs.fetchurl {
      url = "https://opensource.apple.com/tarballs/cctools/cctools-${version}.tar.gz";
      sha256 = "1dsw1jhkfcm1x1vyhhpsg1bl1306v1rdvdxvfspgj5sild7h6rnf";
    };

    patches = [ ./cctools_megapatch.patch ];

    CFLAGS =
      "-I../cctools/include " +
      "-isystem ${sdk}/usr/include " +

      "-Wfatal-errors " +
      "-Werror -Wno-deprecated-declarations -Wno-deprecated " +

      "-D__private_extern__= " +
      "-D__LITTLE_ENDIAN__";
  };

  ld = native.make_derivation rec {
    name = "ld64-${version}-${host}";
    version = "274.2";
    inherit host arch;
    src = nixpkgs.fetchurl {
      url = "https://opensource.apple.com/tarballs/ld64/ld64-${version}.tar.gz";
      sha256 = "1mzp2sszcvg86b1jb90prhcrwk7g7inikr7plnklk7g93728jp8p";
    };
    patches = [ ./ld64_megapatch.patch ];
    builder = ./ld_builder.sh;
    native_inputs = [ clang ];
    CXXFLAGS =
      "-Werror " +
      "-Wfatal-errors " +
      "-std=gnu++11 " +
      "-Iinclude " +
      "-I../ld64/src/ld " +
      "-I../ld64/src/abstraction " +
      "-isystem ${sdk_lite}/usr/include " +
      "-D__LITTLE_ENDIAN__";
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

  macos_version_min = "10.11";

  toolchain = native.make_derivation rec {
    name = "mac-toolchain";
    builder = ./builder.sh;
    inherit host sdk;
    wrapper = ./wrapper;
    native_inputs = [ clang ld xar ];

    CXXFLAGS =
      "-std=c++11 " +
      "-Wall " +
      "-I. " +
      "-O2 -g " +
      "-DWRAPPER_OS_VERSION_MIN=\\\"${macos_version_min}\\\" " +
      "-DWRAPPER_SDK_PATH=\\\"/nix/store/shs3mnp6j07sv2xzzs92a4ydbvb6fs0w-macos-sdk\\\" " +
      "-DWRAPPER_HOST=\\\"${host}\\\" " +
      "-DWRAPPER_ARCH=\\\"${arch}\\\" " +
      "-DWRAPPER_PATH=\\\"${cctools}/bin:${clang}/bin\\\"";
    # TODO: use cctools.version for the -mlinker-version argument to clang
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

    inherit clang tapi ld cctools cctools_tpoechtrager xar sdk sdk_lite;

    make_derivation = import ../make_derivation.nix nixpkgs crossenv;
  };
in
  crossenv
