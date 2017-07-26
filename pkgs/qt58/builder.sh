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

../qt/configure -prefix $out $configure_flags

make

make install
