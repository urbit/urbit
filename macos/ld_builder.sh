source $setup

tar -xf $src
mv ld64-* ld64

cd ld64
for patch in $patches; do
  echo applying patch $patch
  patch -p1 -i $patch
done
cd ..

mkdir build
cd build

mkdir include

{
  echo "#define SUPPORT_ARCH_$arch 1"
  echo "#define ALL_SUPPORTED_ARCHS \"$arch\""
} > include/configure.h

g++ $CXXFLAGS ../ld64/src/ld/{ld,Options}.cpp
