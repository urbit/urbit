echo $stdenv

source $stdenv/setup

tar -xf $src
mv p-load-* p-load

mkdir build
cd build

cmake ../p-load \
  -DCMAKE_TOOLCHAIN_FILE=$cmake_toolchain \
  -DCMAKE_INSTALL_PREFIX=$out

make

make install
