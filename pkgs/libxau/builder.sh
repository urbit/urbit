source $setup

tar -xf $src
mv libXau-* libxau

mkdir build
cd build

PKG_CONFIG=pkg-config-cross \
../libxau/configure --prefix=$out $configure_flags

make

make install

ln -s $xorgproto/lib/pkgconfig/xproto.pc $out/lib/pkgconfig/
