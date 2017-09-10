{ native }:
let
  nixpkgs = native.nixpkgs;

  arch = "x86_64";

  host = "${arch}-apple-darwin15";

  os = "macos";

  compiler = "clang";

  exe_suffix = "";

  clang_version = "3.9.1";

  clang_src = nixpkgs.fetchurl {
    url = "https://llvm.org/releases/${clang_version}/cfe-${clang_version}.src.tar.xz";
    sha256 = "0qsyyb40iwifhhlx9a3drf8z6ni6zwyk3bvh0kx2gs6yjsxwxi76";
  };

  llvm_src = nixpkgs.fetchurl {
    url = "https://llvm.org/releases/${clang_version}/llvm-${clang_version}.src.tar.xz";
    sha256 = "1vi9sf7rx1q04wj479rsvxayb6z740iaz3qniwp266fgp5a07n8z";
  };

  clang = native.make_derivation rec {
    name = "mac-clang";
    builder = ./clang_builder.sh;
    osxcross = ./osxcross;
    version = clang_version;
    src = llvm_src;
    inherit llvm_src;
    native_inputs = [ nixpkgs.python2 ];
  };

  toolchain = native.make_derivation rec {
    name = "mac-toolchain";
    builder = ./builder.sh;
    inherit host;

    osxcross = ./osxcross;

    sdk = ./macsdk.tar.xz;

    cctools_src = nixpkgs.fetchurl {
      url = "https://github.com/tpoechtrager/osxcross/raw/474f359/tarballs/cctools-895-ld64-274.2_8e9c3f2.tar.xz";
      sha256 = "0905qhkwismr6bjbzmjbjxgg72ib5a46lfwglw1fzsx3swmzfaqj";
    };

    xar_src = nixpkgs.fetchurl {
      url = "https://github.com/tpoechtrager/osxcross/raw/474f359/tarballs/xar-1.6.1.tar.gz";
      sha256 = "0qxvzxz0pddkmswx4w09ma9gvx8a0ch4ki7pdxkwg7sg3z5z0x5n";
    };

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
