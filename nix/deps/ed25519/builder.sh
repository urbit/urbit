source $stdenv/setup

sources=" \
  $src/src/add_scalar.c \
  $src/src/seed.c \
  $src/src/verify.c \
  $src/src/add_scalar.c \
  $src/src/sha512.c \
  $src/src/ge.c \
  $src/src/fe.c \
  $src/src/keypair.c \
  $src/src/sign.c \
  $src/src/sc.c \
  $src/src/key_exchange.c \
"

CFLAGS="-O3 -Wall -I$src/src"

for fn in $sources
do echo $CC $CFLAGS -c $fn -o $(basename $fn).o
   $CC -O3 -Wall -I$src/src -c $fn -o $(basename $fn).o
done

mkdir -p $out/{lib,include}

$AR rcs $out/lib/libed25519.a *.o
echo $AR rcs $out/lib/libed25519.a *.o

cp $src/src/*.h $out/include
