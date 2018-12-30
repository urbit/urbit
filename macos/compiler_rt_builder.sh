source $setup

tar -xf $src
mv compiler-rt-* src

mkdir build
cd build
cmake ../src -GNinja -DCMAKE_INSTALL_PREFIX=$out
ninja
ninja install