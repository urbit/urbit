{ nixpkgs, arch }:

let
  host = "${arch}-w64-mingw32";

  binutils = import ./binutils { inherit nixpkgs host; };

  mingw-w64_info = rec {
    name = "mingw-w64-${version}";
    version = "2017-03-10";
    src = nixpkgs.fetchgit {
      url = "git://git.code.sf.net/p/mingw-w64/mingw-w64";
      rev = "c2f24277dbc6878b98a5f5174a01b103d16d5bcd";
      sha256 = "1r8pbq0z0x8lchwm434x81grf3g8sq488bq2cr2w6pr0sp5cxcn8";
    };
    patches = [ ];
  };

  mingw-w64_headers = nixpkgs.stdenv.mkDerivation {
    name = "${mingw-w64_info.name}-headers";
    inherit (mingw-w64_info) src patches;
    preConfigure = "cd mingw-w64-headers";
    configureFlags = "--without-crt --enable-secure-api";
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

  os = "windows";

  exe_suffix = ".exe";

  gyp_os = "win";

in
{
  # Target info
  inherit host arch os exe_suffix;

  # Toolchain
  inherit gcc binutils mingw-w64_full;

  # nixpkgs: a wide variety of programs and build tools
  inherit nixpkgs;

  # Expressions used to bootstrap the toolchain, not normally needed.
  inherit mingw-w64_info mingw-w64_headers gcc_stage_1;

  # Support for various build tools
  inherit cmake_toolchain gyp_os;
}
