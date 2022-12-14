{ stdenv, sources, cmake, openssl, libh2o, enableParallelBuilding ? true }:

stdenv.mkDerivation {
  name = "libwslay";
  version = sources.wslay.rev;
  src = sources.wslay;
  # patches = [ ./cmakefiles_static.patch ];

  nativeBuildInputs = [ cmake ];
  buildInputs = [ openssl libh2o ];

  cmakeFlags = [
    "-DBUILD_SHARED_LIBS=OFF"
  ];

  inherit enableParallelBuilding;
}
