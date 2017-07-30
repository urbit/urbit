source $setup

tar -xf $src
mv libxcb-* libxcb

cd libxcb
for patch in $patches; do
  echo applying patch $patch
  patch -p1 -i $patch
done
cd ..

mkdir build
cd build

PKG_CONFIG=pkg-config-cross \
../libxcb/configure --prefix=$out $configure_flags

make

make install

# Make static linking work.
sed -i 's/Requires.private/Requires/' $out/lib/pkgconfig/*.pc

ln -sv $libxau/lib/pkgconfig/*.pc $out/lib/pkgconfig/
