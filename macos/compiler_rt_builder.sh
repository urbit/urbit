source $setup

tar -xf $src
mv compiler-rt-* src

mkdir build
cd build
cmake ../src -GNinja -DCMAKE_INSTALL_PREFIX=$out $cmake_flags \
  -DCMAKE_CFLAGS="-target ${host} --sysroot ${sdk}" \
  -DCMAKE_CXXFLAGS="-target ${host} --sysroot ${sdk}"

ninja
ninja install