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

set -x
ln -sv $xproto/lib/pkgconfig/*.pc $out/lib/pkgconfig/
ln -sv $kbproto/lib/pkgconfig/*.pc $out/lib/pkgconfig/
ln -sv $libxcb/lib/pkgconfig/*.pc $out/lib/pkgconfig/
