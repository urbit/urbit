{ native, arch }:

let
  nixpkgs = native.nixpkgs;

  host = "${arch}-w64-mingw32";

  binutils = import ./binutils { inherit native host; };

  mingw-w64_info = rec {
    name = "mingw-w64-${version}";
    version = "2018-10-18";
    src = nixpkgs.fetchgit {
      url = "git://git.code.sf.net/p/mingw-w64/mingw-w64";
      rev = "c69c7a706d767c5ca3c7d1c70887fcd8e1f940b3";
      sha256 = "028g8wpy56m3nlna6dnzsz66355kn6gqmwmkm9raas9s1ayzv5il";
    };
    patches = [
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

  crossenv = rec {
    is_cross = true;

    # Target info.
    inherit host arch;
    os = "windows";
    compiler = "gcc";
    exe_suffix = ".exe";
    cmake_system = "Windows";
    meson_system = "windows";
    meson_cpu_family =
      if arch == "i686" then "x86"
      else if arch == "x86_64" then "x86_64"
      else throw "not sure what meson_cpu_family code to use";
    meson_cpu = arch;

    # Build tools.
    inherit nixpkgs native;
    wrappers = import ../wrappers crossenv;

    # License information that should be shipped with any software
    # compiled by this environment.
    inherit global_license_set;

    # Handy shortcuts.
    inherit gcc binutils;
    inherit mingw-w64_full mingw-w64_info mingw-w64_headers gcc_stage_1;
    mingw-w64 = mingw-w64_full;

    # Build tools available on the PATH for every derivation.
    default_native_inputs = native.default_native_inputs
      ++ [ gcc binutils wrappers ];

    make_derivation = import ../make_derivation.nix crossenv;
  };
in
  crossenv
