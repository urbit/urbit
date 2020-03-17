source $setup

tar -xf $src
mv expat-* expat

cd expat
for patch in $patches; do
  echo applying patch $patch
  patch -p1 -i $patch
done
cd ..

mkdir build
cd build

../expat/configure \
  --prefix=$out --host=$host \
  --enable-static --disable-shared

make

make install

mkdir $out/license
cp ../expat/COPYING $out/license/LICENSE
