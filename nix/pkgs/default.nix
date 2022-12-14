{ stdenv, sources, cmake, openssl, enableParallelBuilding ? true }:

stdenv.mkDerivation {
  name = "libwslay";
  version = sources.wslay.rev;
  src = sources.wslay;
  patches = [ ./cmakefiles_static.patch ];

  nativeBuildInputs = [ cmake ];
  buildInputs = [ openssl ];

  cmakeFlags = [
    "-DBUILD_SHARED_LIBS=OFF"
  ];

  inherit enableParallelBuilding;
}
