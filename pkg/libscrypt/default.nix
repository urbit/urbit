{ stdenv, sources }:

stdenv.mkDerivation {
  pname = "libscrypt";
  version = sources.libscrypt.rev;
  src = sources.libscrypt;

  buildPhase = ''
    sources=" \
      crypto_scrypt-check \
      crypto_scrypt-hash \
      crypto_scrypt-hexconvert \
      crypto_scrypt-nosse \
      crypto-mcf \
      crypto-scrypt-saltgen \
      slowequals \
      sha256 \
      b64 \
    "

    CFLAGS="-I$src -Wall -ffast-math -O3 -D_FORTIFY_SOURCE=2 -fstack-protector"

    for s in $sources; do 
       $CC $CFLAGS -c $src/$s.c -o $s.o
    done

    $AR rcs libscrypt.a *.o
  '';

  installPhase = ''
    mkdir -p $out/{lib,include}
    cp libscrypt.a $out/lib
    cp $src/*.h $out/include/
  '';
}
