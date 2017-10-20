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

CFLAGS="$CFLAGS $(pkg-config libtapi --cflags)"

mkdir include

{
  echo "#define SUPPORT_ARCH_$arch 1"
  echo "#define ALL_SUPPORTED_ARCHS \"$arch\""
} > include/configure.h

for f in ../ld64/src/ld/{ld,Options}.cpp; do
  echo "compiling $f"
  g++ -c $CFLAGS $f -o $(basename $f).o
done

g++ *.o -o $host-ld

mkdir -p $out/bin
cp $host-ld $out/bin
