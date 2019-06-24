source $stdenv/setup

CFLAGS="-O3 -Wall -ffast-math -Wno-unused-const-variable"

echo $CC $CFLAGS -c $src/src/tls.c -o tls.o
$CC $CFLAGS -c $src/src/tls.c -o tls.o

echo $AR rcs libsni.a tls.o
$AR rcs libsni.a tls.o

mkdir -p $out/{lib,include}
cp libsni.a $out/lib/
cp $src/src/tls.h $out/include/
