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

#tar -xf $tapi_src
#mv tapi-* tapi
#mv tapi llvm/projects/

# Hacky workaround to allow tapi to find clang headers.  Maybe clang should be
# patched so that it sets the INTERFACE_INCLUDE_DIRS property on its clangBasic
# target instead.
#ln -s $PWD/llvm/projects/clang/include/clang llvm/projects/tapi/include/
#ln -s $PWD/build/projects/clang/include/clang llvm/include/

mkdir build
cd build

cmake ../llvm -GNinja -DDEFAULT_SYSROOT=$out -DCMAKE_INSTALL_PREFIX=$out $cmake_flags

#ninja projects/clang/include/clang/Basic/DiagnosticCommonKinds.inc

#ninja projects/tapi/lib/Core/CMakeFiles/tapiCore.dir/Configuration.cpp.o
#ninja projects/tapi/lib/Core/CMakeFiles/tapiCore.dir/Architecture.cpp.o
#ninja TapiDriverOptions
#ninja libtapi tapi tapi-headers

ninja

ninja install
