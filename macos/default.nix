{ native }:
let
  nixpkgs = native.nixpkgs;

  arch = "x86_64";

  darwin_name = "darwin15";

  host = "${arch}-apple-${darwin_name}";

  os = "macos";

  compiler = "clang";

  exe_suffix = "";

  llvm_version = "5.0.0";

  clang_src = nixpkgs.fetchurl {
    url = "https://llvm.org/releases/${llvm_version}/cfe-${llvm_version}.src.tar.xz";
    sha256 = "0w09s8fn3lkn6i04nj0cisgp821r815fk5b5fjn97xrd371277q1";
  };

  llvm_src = nixpkgs.fetchurl {
    url = "https://llvm.org/releases/${llvm_version}/llvm-${llvm_version}.src.tar.xz";
    sha256 = "1nin64vz21hyng6jr19knxipvggaqlkl2l9jpd5czbc4c2pcnpg3";
  };

  osxcross = ./osxcross;

  cctools_src = nixpkgs.fetchurl {
    url = "https://github.com/tpoechtrager/cctools-port/archive/22ebe72.tar.gz";
    sha256 = "1pmn2iyw00ird3ni53wl05p3lm3637jyfmq393fx59495wnyxpgf";
  };

  cctools = native.make_derivation {
    name = "cctools";
    builder = ./cctools_builder.sh;
    src = cctools_src;
    configure_flags = "--target=${host}";
    native_inputs = [ nixpkgs.clang ];
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

  clang = native.make_derivation {
    name = "clang";
    builder = ./clang_builder.sh;
    version = llvm_version;
    src = clang_src;
    inherit llvm_src;
    patches = [ ./clang_megapatch.patch ];
    native_inputs = [ nixpkgs.python2 ];
    cmake_flags =
      "-DCMAKE_BUILD_TYPE=Release " +
      # "-DCMAKE_BUILD_TYPE=Debug " +
      "-DLLVM_ENABLE_ASSERTIONS=OFF";
  };

  sdk = native.make_derivation rec {
    name = "macos-sdk";
    builder = ./sdk_builder.sh;
    version = "10.11";
    src = ./MacOSX10.11.sdk.tar.xz;
  };

  toolchain = native.make_derivation rec {
    name = "mac-toolchain";
    builder = ./builder.sh;
    inherit host osxcross sdk;
    native_inputs = [ clang cctools xar ];

    OSX_VERSION_MIN = "10.11";  # was 10.5
    SDK_VERSION = sdk.version;
    TARGET = darwin_name;
    X86_64H_SUPPORTED = true;

    OSXCROSS_VERSION = "0.15";
    OSXCROSS_OSX_VERSION_MIN = OSX_VERSION_MIN;
    OSXCROSS_TARGET = darwin_name;
    OSXCROSS_SDK_VERSION = SDK_VERSION;
    OSXCROSS_SDK = sdk;
    OSXCROSS_CCTOOLS_PATH = "${cctools}/bin";
    OSXCROSS_LIBLTO_PATH = "";
    OSXCROSS_LINKER_VERSION = "274.2";
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
    toolchain_inputs = [ toolchain ];

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
