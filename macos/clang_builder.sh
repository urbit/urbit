source $setup

tar -xf $llvm_src
mv llvm-* llvm

tar -xf $lld_src
mv lld-* lld
mv lld llvm/tools/

tar -xf $src
mv cfe-* clang
cd clang
for patch in $patches; do
  echo applying patch $patch
  patch -p1 -i $patch
done
cd ..
mv clang llvm/projects/

mkdir build
cd build

cmake ../llvm -GNinja -DDEFAULT_SYSROOT=$out -DCMAKE_INSTALL_PREFIX=$out $cmake_flags

ninja

ninja install
