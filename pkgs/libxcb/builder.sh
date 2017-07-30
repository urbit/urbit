source $setup

tar -xf $src
mv libxcb-* libxcb

cd libxcb
for patch in $patches; do
  echo applying patch $patch
  patch -p1 -i $patch
done
autoreconf -vfi
cd ..

mkdir build
cd build

PKG_CONFIG=pkg-config-cross \
../libxcb/configure --prefix=$out $configure_flags
# cat config.log

make

make install
