echo $stdenv

source $stdenv/setup

tar -xf $src

mkdir build
cd build

cmake ../p-load-$version \
  -DCMAKE_TOOLCHAIN_FILE=$cmake_toolchain \
  -DCMAKE_INSTALL_PREFIX=$out

make

make install
