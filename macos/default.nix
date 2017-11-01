# TODO: remove unused things: sdk_lite, common_crypto, ld_apple, xar (?)

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

    version = "5.0.0";

    src = nixpkgs.fetchurl {
      url = "https://llvm.org/releases/${version}/cfe-${version}.src.tar.xz";
      sha256 = "0w09s8fn3lkn6i04nj0cisgp821r815fk5b5fjn97xrd371277q1";
    };

    llvm_src = nixpkgs.fetchurl {
      url = "https://llvm.org/releases/${version}/llvm-${version}.src.tar.xz";
      sha256 = "1nin64vz21hyng6jr19knxipvggaqlkl2l9jpd5czbc4c2pcnpg3";
    };

    lld_src = nixpkgs.fetchurl {
      url = "http://releases.llvm.org/${version}/lld-${version}.src.tar.xz";
      sha256 = "15rqsmfw0jlsri7hszbs8l0j7v1030cy9xvvdb245397llh7k6ir";
    };

    patches = [ ./clang_megapatch.patch ];

    builder = ./clang_builder.sh;

    native_inputs = [ nixpkgs.python2 ];

    cmake_flags =
      "-DCMAKE_BUILD_TYPE=Release " +
      # "-DCMAKE_BUILD_TYPE=Debug " +
      "-DLLVM_TARGETS_TO_BUILD=X86\;ARM " +
      "-DLLVM_ENABLE_RTTI=ON " +  # ld64 uses dynamic_cast, requiring rtti
      "-DLLVM_ENABLE_ASSERTIONS=OFF";
  };

  # Note: There is an alternative version we could use, but it
  # has a copy of LLVM in it: https://github.com/tpoechtrager/apple-libtapi
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

  ld_tpoechtrager = native.make_derivation rec {
    name = "ld-tpoechtrager-${version}";
    version = "c1cc758";
    apple_version = "274.2";  # from README.md
    inherit host;
    src = nixpkgs.fetchurl {
      url = "https://github.com/tpoechtrager/cctools-port/archive/${version}.tar.gz";
      sha256= "11bfcndzbdmjp2piabyqs34da617fh5fhirqvb9w87anfan15ffa";
    };
    patches = [
      ./ld_tpoechtrager_format.patch
      ./ld_tpoechtrager_megapatch.patch
    ];
    builder = ./ld_tpoechtrager_builder.sh;
    native_inputs = [ tapi ];
  };
  ld = ld_tpoechtrager;

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

  common_crypto = native.make_derivation rec {
    name = "common-crypto-${version}";
    version = "60118.1.1";
    src = nixpkgs.fetchurl {
      url = "https://opensource.apple.com/tarballs/CommonCrypto/CommonCrypto-${version}.tar.gz";
      sha256 = "1iw2231pw1hp66wb01vlqrhmj6gzwlkjisl1v6axgq0pwyrkq34h";
    };
    builder = ./common_crypto_builder.sh;
    CFLAGS =
      "-O2 " +
      "-Werror " +
      "-Wfatal-errors " +
      "-I../cc/include " +
      "-isystem ${sdk_lite}/include";
  };

  ld_apple = native.make_derivation rec {
    name = "ld64-${version}-${host}";
    version = "274.2";
    inherit host arch;
    src = nixpkgs.fetchurl {
      url = "https://opensource.apple.com/tarballs/ld64/ld64-${version}.tar.gz";
      sha256 = "1mzp2sszcvg86b1jb90prhcrwk7g7inikr7plnklk7g93728jp8p";
    };
    patches = [ ./ld64_megapatch.patch ];
    builder = ./ld_builder.sh;
    native_inputs = [ common_crypto tapi nixpkgs.libuuid.dev ];
    CFLAGS =
      "-O2 " +
      "-Wno-unused-result " +
      "-Werror " +
      "-Wfatal-errors " +
      "-Iinclude " +
      "-I../ld64/src/ld " +
      "-I../ld64/src/ld/parsers " +
      "-I../ld64/src/abstraction " +
      "-I${nixpkgs.libuuid.dev}/include " +
      "-isystem ${sdk_lite}/include " +
      "-D__LITTLE_ENDIAN__ " +
      "-D_DARWIN_C_SOURCE";
  };

  xar = native.make_derivation {
    name = "xar";
    builder = ./xar_builder.sh;
    src = nixpkgs.fetchurl {
      url = "https://github.com/downloads/mackyle/xar/xar-1.6.1.tar.gz";
      sha256 = "0ghmsbs6xwg1092v7pjcibmk5wkyifwxw6ygp08gfz25d2chhipf";
    };
    native_inputs = [
      nixpkgs.libxml2.dev
      nixpkgs.openssl.dev
      nixpkgs.zlib.dev
    ];
  };

  toolchain = native.make_derivation rec {
    name = "mac-toolchain";
    builder = ./builder.sh;
    inherit host sdk;
    wrapper = ./wrapper;
    native_inputs = [ ld clang ];
    inherit clang;

    CXXFLAGS =
      "-std=c++11 " +
      "-Wall " +
      "-I. " +
      "-O2 -g " +
      "-DWRAPPER_OS_VERSION_MIN=\\\"${macos_version_min}\\\" " +
      "-DWRAPPER_HOST=\\\"${host}\\\" " +
      "-DWRAPPER_ARCH=\\\"${arch}\\\" " +
      "-DWRAPPER_SDK_PATH=\\\"${sdk}\\\" " +
      "-DWRAPPER_SDK_VERSION=\\\"${sdk.version}\\\" " +
      "-DWRAPPER_LINKER_VERSION=\\\"${ld.apple_version}\\\" " +
      "-DWRAPPER_PATH=\\\"${ld}/bin:${clang}/bin\\\"";
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
    inherit host arch os compiler exe_suffix;

    # CMake toolchain file.
    inherit cmake_toolchain;

    # A wide variety of programs and build tools.
    inherit nixpkgs;

    # Some native build tools made by nixcrpkgs.
    inherit native;

    # Make it easy to build or refer to the build tools.
    inherit clang common_crypto tapi sdk sdk_lite ld xar toolchain;

    make_derivation = import ../make_derivation.nix crossenv;
  };
in
  crossenv
