source $setup

tar -xf $src
mv p-load-* p-load

cd p-load
for patch in $patches; do
  echo applying patch $patch
  patch -p1 -i $patch
done
cd ..

cat p-load/CMakeLists.txt

mkdir build
cd build

cmake-cross ../p-load \
  -DCMAKE_INSTALL_PREFIX=$out

make VERBOSE=Y

make install
