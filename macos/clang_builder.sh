source $setup

tar -xf $llvm_src
mv llvm-* llvm

tar -xf $src
mv cfe-* clang
cd clang
for patch in $patches; do
  echo applying patch $patch
  patch -p1 -i $patch
done
cd ..
mv clang llvm/projects/

tar -xf $tapi_src
mv tapi-* tapi
mv tapi llvm/projects/

mkdir build
cd build

# -DLLVM_ENABLE_PROJECTS="clang;tapi"

cmake -C $llvm_cache ../llvm -GNinja -DDEFAULT_SYSROOT=$out -DCMAKE_INSTALL_PREFIX=$out $cmake_flags

ninja libtapi tapi tapi-headers

ninja

ninja install
