source $setup

tar -xf $src
mv compiler-rt-* src

cd src
for patch in $patches; do
  echo applying patch $patch
  patch -p1 -i $patch
done
cd ..

mkdir build
cd build
cmake ../src -GNinja -DCMAKE_INSTALL_PREFIX=$out $cmake_flags

ninja
ninja install