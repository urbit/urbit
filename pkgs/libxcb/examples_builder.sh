source $setup

tar -xf $src
mv xcb-demo-* demo

mkdir build
cd build

PKG_CONFIG=pkg-config-cross \
../demo/configure --prefix=$out $configure_flags

make

make install
