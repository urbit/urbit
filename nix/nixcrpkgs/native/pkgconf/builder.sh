source $setup

tar -xf $src

mkdir build

cd build

../pkgconf-$version/configure \
  --prefix=$out \
  --with-system-libdir=/no-system-libdir/ \
  --with-system-includedir=/no-system-includedir/

make

make install

ln -s $out/bin/pkgconf $out/bin/pkg-config

mkdir $out/license
cp ../pkgconf-$version/COPYING $out/license/LICENSE
