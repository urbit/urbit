source $setup

tar -xf $src
mv cctools-port-* cctools-port

cd cctools-port

for patch in $patches; do
  echo applying patch $patch
  patch -p1 -i $patch
done

# Similar to but not the same as the other _structs.h.
rm cctools/include/foreign/mach/i386/_structs.h

cd ..

mv cctools-port/cctools/ld64 ld64
mv cctools-port/cctools/include include
rm -r cctools-port
rm -r ld64/src/other

mkdir build
cd build

CFLAGS="-Wno-deprecated -Wno-deprecated-declarations -Wno-unused-result -Werror -Wfatal-errors -O2 -g -I../ld64/src -I../ld64/src/ld -I../ld64/src/ld/parsers -I../ld64/src/abstraction -I../ld64/src/3rd -I../ld64/src/3rd/include -I../ld64/src/3rd/BlocksRuntime -I../include -I../include/foreign -DTAPI_SUPPORT -DPROGRAM_PREFIX=\\\"$host-\\\" -D__LITTLE_ENDIAN__ -D__private_extern__= $(pkg-config --cflags libtapi)"

CXXFLAGS="-std=gnu++11 $CFLAGS"

LDFLAGS="$(pkg-config --libs libtapi) -ldl -lpthread"

for f in ../ld64/src/ld/*.c ../ld64/src/3rd/*.c; do
  echo "compiling $f"
  eval "gcc -c $CFLAGS $f -o $(basename $f).o"
done

for f in $(find ../ld64/src -name \*.cpp); do
  echo "compiling $f"
  eval "g++ -c $CXXFLAGS $f -o $(basename $f).o"
done

g++ *.o $LDFLAGS -o $host-ld

mkdir -p $out/bin
cp $host-ld $out/bin
