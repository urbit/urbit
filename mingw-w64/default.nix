{ nixpkgs, arch }:
let
  binutils = import ./binutils { inherit nixpkgs arch; };

  mingw-w64 = rec {
    name = "mingw-w64-${version}";
    version = "4.0.6";
    src = nixpkgs.fetchurl {
      url = "mirror://sourceforge/mingw-w64/mingw-w64-v${version}.tar.bz2";
      sha256 = "0p01vm5kx1ixc08402z94g1alip4vx66gjpvyi9maqyqn2a76h0c";
    };
  };

  mingw-w64_headers = nixpkgs.stdenv.mkDerivation {
    name = "${mingw-w64.name}-headers";
    src = mingw-w64.src;
    preConfigure = "cd mingw-w64-headers";
    configureFlags = "--without-crt";
  };

  gcc_stage_1 = import ./gcc {
    stage = 1;
    libc = mingw-w64_headers;
    inherit nixpkgs arch binutils;
  };

  mingw-w64_crt_and_headers = nixpkgs.stdenv.mkDerivation {
    name = mingw-w64.name;
    src = mingw-w64.src;
    buildInputs = [ gcc_stage_1 ];
    configureFlags = "--host=${arch}-w64-mingw32";
  };

  gcc = import ./gcc {
    libc = mingw-w64_crt_and_headers;
    inherit nixpkgs arch binutils;
  };

in {
  host = "${arch}-w64-mingw32";
  inherit binutils nixpkgs arch;
  inherit mingw-w64 mingw-w64_headers mingw-w64_crt_and_headers gcc_stage_1 gcc;
}
