source $setup

tar -xf $src
mv pkgconf-* pkgconf

cd pkgconf
for patch in $patches; do
  echo applying patch $patch
  patch -p1 -i $patch
done
./autogen.sh
cd ..

mkdir build
cd build

../pkgconf/configure --prefix=$out

make

make install

ln -s $out/bin/pkgconf $out/bin/pkg-config

mkdir $out/license
cp ../pkgconf/COPYING $out/license/LICENSE
