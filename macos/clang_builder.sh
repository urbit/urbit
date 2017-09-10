source $setup

tar -xf $llvm_src
mv llvm-* llvm
cd llvm/tools
tar -xf $src
mv cfe-* clang
cd ../..

mkdir build
cd build

cmake ../llvm -DCMAKE_INSTALL_PREFIX=$out $cmake_flags

make

make install
