{ nixpkgs, arch }:
let
  binutils = import ./binutils { inherit nixpkgs arch; };

  mingw_w64 = rec {
    version = "4.0.6";
    src = nixpkgs.fetchurl {
      url = "mirror://sourceforge/mingw-w64/mingw-w64-v${version}.tar.bz2";
      sha256 = "0p01vm5kx1ixc08402z94g1alip4vx66gjpvyi9maqyqn2a76h0c";
    };
  };

  mingw_w64_headers = nixpkgs.stdenv.mkDerivation {
    name = "mingw-w64-${mingw_w64.version}-headers";
    src = mingw_w64.src;
    preConfigure = "cd mingw-w64-headers";
    configureFlags = "--without-crt";
  };

  gcc_stage_1 = import ./gcc {
    stage = 1;
    libc = mingw_w64_headers;
    inherit nixpkgs arch binutils;
  };

  gcc_stage_2 = import ./gcc {
    stage = 2;
    libc = mingw_w64_headers;
    inherit nixpkgs arch binutils;
  };

  gcc = import ./gcc {
    libc = mingw_w64;
    inherit nixpkgs arch binutils;
  };

in {
  host = "${arch}-w64-mingw32";
  inherit binutils nixpkgs arch;
  inherit mingw_w64 mingw_w64_headers gcc_stage_1 gcc_stage_2 gcc;
}
