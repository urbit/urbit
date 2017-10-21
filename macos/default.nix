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
    version = "${version0}.${version1}.${version2}";
    version0 = "2";
    version1 = "0";
    version2 = "0";
    src = nixpkgs.fetchurl {
      url = "https://github.com/DavidEGrayson/tapi/archive/f98d0c3.tar.gz";
      sha256 = "0jibz0fsyh47q8y3w6f0qspjh6fhs164rkhjg7x6k7qhlawcdy6g";
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
    native_inputs = [ tapi native.pkgconf ];
    CFLAGS =
      "-fno-rtti " +
      "-O2 " +
      "-Werror " +
      "-Wfatal-errors " +
      "-Iinclude " +
      "-I../ld64/src/ld " +
      "-I../ld64/src/abstraction " +
      "-isystem ${sdk_lite}/usr/include " +
      "-D__LITTLE_ENDIAN__ " +
      "-D_DARWIN_C_SOURCE";
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
      "-DWRAPPER_PATH=\\\"${ld}/bin:${clang}/bin\\\"";
    # TODO: use ld.version for the -mlinker-version argument to clang
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
    toolchain_drvs = [ clang ld xar toolchain ];

    # Build tools and variables to support them.
    inherit cmake_toolchain;

    # nixpkgs: a wide variety of programs and build tools.
    inherit nixpkgs;

    # Some native build tools made by nixcrpkgs.
    inherit native;

    inherit clang tapi sdk sdk_lite ld xar;

    make_derivation = import ../make_derivation.nix nixpkgs crossenv;
  };
in
  crossenv
