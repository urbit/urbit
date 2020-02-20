source $setup

tar -xf $src
mv libX11-* libx11

mkdir build
cd build

PKG_CONFIG=pkg-config-cross \
../libx11/configure --prefix $out $configure_flags

make

make install

# Make static linking work.
sed -i 's/Requires.private/Requires/' $out/lib/pkgconfig/*.pc
sed -i 's/Libs.private/Libs/' $out/lib/pkgconfig/*.pc

ln -s x11-xcb.pc $out/lib/pkgconfig/X11-xcb.pc
ln -s x11.pc $out/lib/pkgconfig/X11.pc

ln -sf $xorgproto/lib/pkgconfig/*.pc $out/lib/pkgconfig/
ln -sf $libxcb/lib/pkgconfig/*.pc $out/lib/pkgconfig/
