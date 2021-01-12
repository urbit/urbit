{ stdenv, sources, enableParallelBuilding ? true }:

stdenv.mkDerivation {
  pname = "argon2u";
  version = sources.argon2u.rev;
  src = sources.argon2u;

  postPatch = ''
    substituteInPlace Makefile --replace 'ar rcs' '$(AR) rcs'
  '';

  buildPhase = ''
    make libargon2.a
  '';

  installPhase = ''
    mkdir -p $out/{lib,include}
    cp libargon2.a $out/lib/
    cp include/argon2.h $out/include/
    cp ./src/blake2/*.h $out/include/
  '';

  makeFlags = [
    "AR=${stdenv.cc.targetPrefix}ar" # Fix cross-compilation
  ];

  NO_THREADS = true;

  inherit enableParallelBuilding;
}
