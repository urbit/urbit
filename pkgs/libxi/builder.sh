source $setup

tar -xf $src
mv libXi-* libxi

mkdir build
cd build

PKG_CONFIG=pkg-config-cross \
../libxi/configure --prefix=$out $configure_flags

make

make install

sed -i 's/Requires.private/Requires/' $out/lib/pkgconfig/*.pc

ln -sf $xorgproto/lib/pkgconfig/*.pc $out/lib/pkgconfig/
ln -sf $libx11/lib/pkgconfig/*.pc $out/lib/pkgconfig/
ln -sf $libxext/lib/pkgconfig/*.pc $out/lib/pkgconfig/
ln -sf $libxfixes/lib/pkgconfig/*.pc $out/lib/pkgconfig/
