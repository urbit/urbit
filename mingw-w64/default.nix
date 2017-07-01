{ nixpkgs, arch }:

let
  host = "${arch}-w64-mingw32";

  binutils = import ./binutils { inherit nixpkgs host; };

  mingw-w64_info = rec {
    name = "mingw-w64-${version}";
    version = "2017-05-10";
    src = nixpkgs.fetchgit {
      url = "git://git.code.sf.net/p/mingw-w64/mingw-w64";
      rev = "4545aa0020ee7a2f0db33a05d5a5501a061d9d8b";
      sha256 = "1ybq1yfqxrya8hf9h5b1kn4r3ld97a5cxl5jbvhyzpy640zfkhih";
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

  cmake_toolchain = import ../cmake_toolchain {
    cmake_system_name = "Windows";
    inherit nixpkgs host;
  };

  pkg-config = import ../pkgconf { inherit nixpkgs; };

  pkg-config-cross = import ../pkg-config-cross { inherit nixpkgs; };

  os = "windows";

  exe_suffix = ".exe";

  gyp_os = "win";

  crossenv_base = {
    # Target info
    inherit host arch os exe_suffix;

    # Toolchain
    inherit gcc binutils mingw-w64_full;

    # Build tools
    inherit pkg-config pkg-config-cross;

    # nixpkgs: a wide variety of programs and build tools
    inherit nixpkgs;

    # Expressions used to bootstrap the toolchain, not normally needed.
    inherit mingw-w64_info mingw-w64_headers gcc_stage_1;

    # Support for various build tools
    inherit cmake_toolchain gyp_os;
  };
in
  {
    make_derivation = import ../make_derivation.nix nixpkgs crossenv_base;
  } // crossenv_base

