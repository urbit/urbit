source $setup

tar -xf $src
mv cctools-port-* cctools-port

cd cctools-port
for patch in $patches; do
  echo applying patch $patch
  patch -p1 -i $patch
done
rm -r cctools/ld64/src/other
cd ..

mv cctools-port/cctools/ld64 ld64
mv cctools-port/cctools/include include
rm -r cctools-port

mkdir build
cd build

CFLAGS="$CFLAGS $(pkg-config --cflags libtapi)"
LDFLAGS="$(pkg-config --libs libtapi) -ldl -lpthread"

for f in ../ld64/src/ld/*.c ../ld64/src/3rd/*.c; do
  echo "compiling $f"
  eval "gcc -c $CFLAGS $f -o $(basename $f).o"
done

for f in $(find ../ld64/src -name \*.cpp); do
  echo "compiling $f"
  eval "g++ -c $CFLAGS $f -o $(basename $f).o"
done

g++ *.o $LDFLAGS -o $host-ld

mkdir -p $out/bin
cp $host-ld $out/bin
