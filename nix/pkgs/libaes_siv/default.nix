{ stdenv, sources, cmake, openssl, enableParallelBuilding ? true }:

stdenv.mkDerivation {
  name = "libaes_siv";
  version = sources.libaes_siv.rev;
  src = sources.libaes_siv;

  nativeBuildInputs = [ cmake ];
  buildInputs = [ openssl ];

  installFlags = [ "PREFIX=$(out)" ];

  inherit enableParallelBuilding;
}
