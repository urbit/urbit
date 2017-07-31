source $setup

tar -xf $src
mv libX11-* libx11

mkdir build
cd build

PKG_CONFIG=pkg-config-cross \
../libx11/configure --prefix $out $configure_flags

make

make install
