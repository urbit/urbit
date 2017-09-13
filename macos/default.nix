{ native }:
let
  nixpkgs = native.nixpkgs;

  arch = "x86_64";

  host = "${arch}-apple-darwin15";

  os = "macos";

  compiler = "clang";

  exe_suffix = "";

  clang_version = "5.0.0";

  clang_src = nixpkgs.fetchurl {
    url = "https://llvm.org/releases/${clang_version}/cfe-${clang_version}.src.tar.xz";
    sha256 = "0w09s8fn3lkn6i04nj0cisgp821r815fk5b5fjn97xrd371277q1";
  };

  llvm_src = nixpkgs.fetchurl {
    url = "https://llvm.org/releases/${clang_version}/llvm-${clang_version}.src.tar.xz";
    sha256 = "1nin64vz21hyng6jr19knxipvggaqlkl2l9jpd5czbc4c2pcnpg3";
  };

  osxcross = ./osxcross;

  sdk = ./macsdk.tar.xz;

  cctools_src = nixpkgs.fetchurl {
    url = "https://github.com/tpoechtrager/osxcross/raw/474f359/tarballs/cctools-895-ld64-274.2_8e9c3f2.tar.xz";
    sha256 = "0905qhkwismr6bjbzmjbjxgg72ib5a46lfwglw1fzsx3swmzfaqj";
  };

  xar_src = nixpkgs.fetchurl {
    url = "https://github.com/tpoechtrager/osxcross/raw/474f359/tarballs/xar-1.6.1.tar.gz";
    sha256 = "0ghmsbs6xwg1092v7pjcibmk5wkyifwxw6ygp08gfz25d2chhipf";
  };

  # TODO: prevent it from finding and using /..//bin/ld when I compile a test program
  # TODO: prevent it from finding and using /usr/bin/i686-w64-mingw32-ld
  #  when I use "-target i686-w64-mingw32"
  # TODO: Disable setting the -dynamic-linker automatically (see nixpkgs purity.patch)
  clang = native.make_derivation rec {
    name = "clang";
    builder = ./clang_builder.sh;
    version = clang_version;
    src = clang_src;
    inherit llvm_src;
    patches = [ ./clang_megapatch.patch ];
    native_inputs = [ nixpkgs.python2 ];
    cmake_flags =
      "-DCMAKE_BUILD_TYPE=Release " +
      # "-DCMAKE_BUILD_TYPE=Debug " +
      "-DLLVM_ENABLE_ASSERTIONS=OFF";
  };

  toolchain = native.make_derivation rec {
    name = "mac-toolchain";
    builder = ./builder.sh;
    inherit host osxcross sdk cctools_src xar_src;
    native_inputs = [ clang ];
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

    inherit clang;

    make_derivation = import ../make_derivation.nix nixpkgs crossenv;
  };
in
  crossenv
