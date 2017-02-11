{ nixpkgs, arch }:
let
  host = "${arch}-w64-mingw32";

  os = "windows";

  gyp_os = "win";

  binutils = import ./binutils { inherit nixpkgs arch; };

  mingw-w64_info = rec {
    name = "mingw-w64-${version}";
    version = "2017-02-08";
    src = nixpkgs.fetchgit {
      url = "git://git.code.sf.net/p/mingw-w64/mingw-w64";
      rev = "5aa73896a3313a354cf6550d99ecd652d0abd3b2";
      sha256 = "160ha26gg5wgdb71x9i3gckxbl3yn11b0cqy86mvnfrraz2dhq6f";
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

in
{
  # Target info
  inherit host arch os;

  # Toolchain
  inherit gcc binutils mingw-w64_full;

  # nixpkgs: a wide variety of programs and build tools
  inherit nixpkgs;

  # Expressions used to bootstrap the toolchain, not normally needed.
  inherit mingw-w64_info mingw-w64_headers gcc_stage_1;

  # Support for various build tools
  inherit cmake_toolchain gyp_os;
}
