source $stdenv/setup

echo $CC -fPIC -O3 -o murmur3.o -c $src/murmur3.c
$CC -fPIC -O3 -o murmur3.o -c $src/murmur3.c

mkdir -p $out/{lib,include}

echo $AR rcs $out/lib/libmurmur3.a murmur3.o
$AR rcs $out/lib/libmurmur3.a murmur3.o

cp $src/murmur3.h $out/include
