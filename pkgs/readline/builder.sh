source $stdenv/setup

export CC="${host}-gcc"
export AR="${host}-ar"
export RANLIB="${host}-ranlib"

tar -xf $src

cd readline-$version
for patch in $patches; do
  echo applying patch $patch
  patch -p2 -i $patch
done
cd ..

mkdir build
cd build

../readline-$version/configure \
  --prefix=$out --host=$host \
  --enable-static --disable-shared

make

make install

mkdir $out/license
cp ../readline-$version/COPYING $out/license/LICENSE
