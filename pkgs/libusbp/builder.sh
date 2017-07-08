source $setup

tar -xf $src

cd libusbp-*
for patch in $patches; do
  echo applying patch $patch
  patch -p1 -i $patch
done
cd ..

mkdir build
cd build

cmake-cross ../libusbp-* \
  -DCMAKE_INSTALL_PREFIX=$out \
  -DBUILD_SHARED_LIBS=false

make

make install
