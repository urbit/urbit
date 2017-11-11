source $setup

tar -xf $src
mv p-load-* p-load

mkdir build
cd build

cmake-cross ../p-load \
  -DCMAKE_INSTALL_PREFIX=$out

make

make install
