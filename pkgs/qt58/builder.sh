source $setup

tar -xf $src
mv qtbase-opensource-src-* qt

cd qt
for patch in $patches; do
  echo applying patch $patch
  patch -p1 -i $patch
done
cd ..

mkdir build
cd build

PKG_CONFIG=pkg-config-cross \
../qt/configure -prefix $out $configure_flags

exit 44

make

make install
