source $setup

tar -xf $src
mv pkgconf-* pkgconf

mkdir build

cd build

../pkgconf/configure \
  --prefix=$out \
  --with-system-libdir=/no-system-libdir/ \
  --with-system-includedir=/no-system-includedir/

make

make install

ln -s $out/bin/pkgconf $out/bin/pkg-config

mkdir -p $out/share/aclocal
cp ../pkgconf/pkg.m4 $out/share/aclocal/

mkdir $out/license
cp ../pkgconf/COPYING $out/license/LICENSE
