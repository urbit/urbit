source $stdenv/setup

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

for s in $sources
do echo $CC $CFLAGS -c $src/$s.c -o $s.o
   $CC $CFLAGS -c $src/$s.c -o $s.o
done

echo $AR rcs libscrypt.a *.o
$AR rcs libscrypt.a *.o

mkdir -p $out/{lib,include}
cp libscrypt.a $out/lib
cp $src/*.h $out/include
