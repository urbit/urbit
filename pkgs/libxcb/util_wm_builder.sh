source $setup

tar -xf $src
mv xcb-util-wm-* util

mkdir build
cd build

PKG_CONFIG=pkg-config-cross \
../util/configure --prefix=$out $configure_flags

make

make install
