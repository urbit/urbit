{ nixpkgs, arch }:

let
  host = "${arch}-w64-mingw32";

  binutils = import ./binutils { inherit nixpkgs arch; };

  mingw-w64_info = rec {
    name = "mingw-w64-${version}";
    version = "2017-02-17";
    src = nixpkgs.fetchgit {
      url = "git://git.code.sf.net/p/mingw-w64/mingw-w64";
      rev = "a511b58278c51ba10ae613b9b68c8664219d090d";
      sha256 = "1p5fasxz1rx832wdamj6pdhvvqmhixy3qmbh7f9diwl1sk4a399q";
    };
    patches = [];
  };

  mingw-w64_headers = nixpkgs.stdenv.mkDerivation {
    name = "${mingw-w64_info.name}-headers";
    inherit (mingw-w64_info) src patches;
    preConfigure = "cd mingw-w64-headers";
    configureFlags = "--without-crt";
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
