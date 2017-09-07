source $setup

tar -xf $src
mv xcb-* util

mkdir build
cd build

PKG_CONFIG=pkg-config-cross \
../util/configure --prefix=$out $configure_flags

make

make install

# xcb-util-image-0.4.0/image/xcb_image.c includes <xcb/xcb_aux.h>
echo "Requires: xcb-aux" >> $out/lib/pkgconfig/xcb-image.pc
ln -sf $libxcb/lib/pkgconfig/*.pc $out/lib/pkgconfig/
ln -sf $libxcb_util/lib/pkgconfig/*.pc $out/lib/pkgconfig/

