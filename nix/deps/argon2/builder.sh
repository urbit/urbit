source $stdenv/setup

cp -r $src ./src
chmod -R a+w ./src
cd ./src

sed -i 's|ar rcs|${AR} rcs|' Makefile

make libargon2.a -j4

mkdir -p $out/{lib,include}
cp libargon2.a      $out/lib
cp include/argon2.h $out/include
cp ./src/blake2/*.h $out/include
