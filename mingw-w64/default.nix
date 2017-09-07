{ native, arch }:

let
  nixpkgs = native.nixpkgs;

  host = "${arch}-w64-mingw32";

  binutils = import ./binutils { inherit nixpkgs host; };

  mingw-w64_info = rec {
    name = "mingw-w64-${version}";
    version = "2017-08-03";
    src = nixpkgs.fetchgit {
      url = "git://git.code.sf.net/p/mingw-w64/mingw-w64";
      rev = "6de0055f99ed447ec63c1a650a3830f266a808bd";
      sha256 = "1830rcd0vsbvpr5m1lrabcqh12qrw1flq333b8xrs5b3n542xy2i";
    };
    patches = [
      ./usb.patch
      ./guid-selectany.patch
    ];
    configure_flags = "--enable-secure-api --enable-idl";
  };

  mingw-w64_headers = nixpkgs.stdenv.mkDerivation {
    name = "${mingw-w64_info.name}-headers";
    inherit host;
    inherit (mingw-w64_info) src patches configure_flags;
    builder = ./builder.sh;
    just_headers = true;
  };

  gcc_stage_1 = import ./gcc {
    stage = 1;
    libc = mingw-w64_headers;
    inherit nixpkgs arch binutils;
  };

  mingw-w64_full = nixpkgs.stdenv.mkDerivation {
    name = "${mingw-w64_info.name}-${host}";
    inherit host;
    inherit (mingw-w64_info) version src patches;
    configure_flags =
      "--host=${host} " +
      "--disable-shared --enable-static " +
      mingw-w64_info.configure_flags;
    buildInputs = [ binutils gcc_stage_1 ];
    builder = ./builder.sh;
  };

  gcc = import ./gcc {
    libc = mingw-w64_full;
    inherit nixpkgs arch binutils;
  };

  global_license_fragment = nixpkgs.stdenv.mkDerivation {
    name = "${mingw-w64_info.name}-license-fragment";
    inherit (mingw-w64_info) version src;
    gcc_src = gcc.src;
    builder = ./license_builder.sh;
  };

  cmake_toolchain = import ../cmake_toolchain {
    cmake_system_name = "Windows";
    inherit nixpkgs host;
  };

  os = "windows";

  compiler = "gcc";

  exe_suffix = ".exe";

  gyp_os = "win";

  crossenv = {
    # Target info variables.
    inherit host arch os compiler exe_suffix;

    # Cross-compiling toolchain.
    inherit gcc binutils;
    toolchain_drvs = [ gcc binutils ];
    inherit mingw-w64_full;
    mingw-w64 = mingw-w64_full;

    # Build tools and variables to support them.
    inherit cmake_toolchain gyp_os;

    # nixpkgs: a wide variety of programs and build tools.
    inherit nixpkgs;

    # Some native build tools made by nixcrpkgs.
    inherit native;

    # License information that should be shipped with any software compiled by
    # this environment.
    inherit global_license_fragment;

    # Expressions used to bootstrap the toolchain, not normally needed.
    inherit mingw-w64_info mingw-w64_headers gcc_stage_1;

    make_derivation = import ../make_derivation.nix nixpkgs crossenv;
  };
in
  crossenv
