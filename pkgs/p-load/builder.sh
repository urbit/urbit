source $setup

tar -xf $src
mv p-load-* p-load

cat p-load/CMakeLists.txt

mkdir build
cd build

cmake-cross ../p-load \
  -DCMAKE_INSTALL_PREFIX=$out

make VERBOSE=Y

make install
