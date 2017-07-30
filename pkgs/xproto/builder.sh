source $setup

tar -xf $src
mv xproto-* xproto

mkdir build
cd build

PKG_CONFIG=pkg-config-cross \
../xproto/configure --prefix=$out $configure_flags

make

make install
