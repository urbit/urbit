{ native, arch }:

let
  nixpkgs = native.nixpkgs;

  host = "${arch}-w64-mingw32";

  binutils = import ./binutils { inherit native host; };

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

  mingw-w64_headers = native.make_derivation {
    name = "${mingw-w64_info.name}-headers";
    inherit host;
    inherit (mingw-w64_info) src patches configure_flags;
    builder = ./builder.sh;
    just_headers = true;
  };

  gcc_stage_1 = import ./gcc {
    stage = 1;
    libc = mingw-w64_headers;
    inherit native arch binutils;
  };

  mingw-w64_full = native.make_derivation {
    name = "${mingw-w64_info.name}-${host}";
    inherit host;
    inherit (mingw-w64_info) version src patches;
    configure_flags =
      "--host=${host} " +
      "--disable-shared --enable-static " +
      mingw-w64_info.configure_flags;
    native_inputs = [ binutils gcc_stage_1 ];
    builder = ./builder.sh;
    just_headers = false;
  };

  gcc = import ./gcc {
    libc = mingw-w64_full;
    inherit native arch binutils;
  };

  license = native.make_derivation {
    name = "${mingw-w64_info.name}-license";
    inherit (mingw-w64_info) version src;
    gcc_src = gcc.src;
    builder = ./license_builder.sh;
  };

  global_license_set = { _global = license; };

  cmake_toolchain = import ../cmake_toolchain {
    cmake_system_name = "Windows";
    inherit nixpkgs host;
  };

  os = "windows";

  compiler = "gcc";

  exe_suffix = ".exe";

  crossenv = {
    is_cross = true;

    default_native_inputs = native.default_native_inputs
      ++ [ gcc binutils native.pkgconf native.wrappers ];

    # Target info variables.
    inherit host arch os compiler exe_suffix;

    # CMake toolchain file.
    inherit cmake_toolchain;

    # A wide variety of programs and build tools.
    inherit nixpkgs;

    # Some native build tools made by nixcrpkgs.
    inherit native;

    # License information that should be shipped with any software compiled by
    # this environment.
    inherit global_license_set;

    # Make it easy to build or refer to the build tools.
    inherit gcc binutils mingw-w64_full mingw-w64_info mingw-w64_headers gcc_stage_1;
    mingw-w64 = mingw-w64_full;

    make_derivation = import ../make_derivation.nix crossenv;
  };
in
  crossenv
