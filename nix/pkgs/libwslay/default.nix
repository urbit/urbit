{ stdenv, sources, cmake, openssl, enableParallelBuilding ? true }:

stdenv.mkDerivation {
  name = "libwslay";
  version = sources.wslay.rev;
  src = sources.wslay;

  nativeBuildInputs = [ cmake ];
  buildInputs = [ openssl ];

  cmakeFlags = [ ];

  inherit enableParallelBuilding;
}
