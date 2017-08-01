source $setup

tar -xf $src
mv libXi-* libxi

mkdir build
cd build

PKG_CONFIG=pkg-config-cross \
../libxi/configure --prefix=$out $configure_flags

make

make install
