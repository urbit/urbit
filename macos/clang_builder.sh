source $setup

tar -xf $llvm_src
mv llvm-* llvm
cd llvm/tools
tar -xf $src
mv cfe-* clang
cd clang
for patch in $patches; do
  echo applying patch $patch
  patch -p1 -i $patch
done
cd ../../..

mkdir build
cd build

cmake ../llvm -GNinja -DDEFAULT_SYSROOT=$out -DCMAKE_INSTALL_PREFIX=$out $cmake_flags

ninja \
  tools/clang/lib/Driver/CMakeFiles/clangDriver.dir/ToolChains.cpp.o \
  tools/clang/lib/Driver/CMakeFiles/clangDriver.dir/Driver.cpp.o

ninja

ninja install
