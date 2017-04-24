source $setup

tar -xf $src

cd expat-$version
for patch in $patches; do
  echo applying patch $patch
  patch -p1 -i $patch
done
cd ..

mkdir build
cd build

../expat-$version/configure \
  --prefix=$out --host=$host \
  --enable-static --disable-shared

make

make install

mv $out/bin/xmlwf $out/bin/xmlwf.exe

mkdir $out/license
cp ../expat-$version/COPYING $out/license/LICENSE
