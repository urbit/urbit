source $setup

tar -xf $src
mv pololu-tic-software-* tic

mkdir build
cd build

cmake-cross ../tic \
  -DCMAKE_INSTALL_PREFIX=$out

make

make install
