{ native }:
let
  nixpkgs = native.nixpkgs;

  arch = "x86_64";

  host = "${arch}-apple-darwin15";

  os = "macos";

  compiler = "clang";

  exe_suffix = "";

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

    native_inputs = [ nixpkgs.clang ];
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

    make_derivation = import ../make_derivation.nix nixpkgs crossenv;
  };
in
  crossenv
