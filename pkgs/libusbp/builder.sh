source $setup

tar -xf $src
mv libusbp-* libusbp

mkdir build
cd build

cmake-cross ../libusbp \
  -DCMAKE_INSTALL_PREFIX=$out \
  -DBUILD_SHARED_LIBS=false

make

make install
