source $setup

tar -xf $src
mv xcb-* util

mkdir build
cd build

PKG_CONFIG=pkg-config-cross \
../util/configure --prefix=$out $configure_flags

make

make install

ln -s $xcb/lib/pkgconfig/{xcb,xcb-render}.pc $out/lib/pkgconfig/
