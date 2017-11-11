source $setup

tar -xf $src
mv cctools-port-* cctools-port

cd cctools-port
for patch in $patches; do
  echo applying patch $patch
  patch -p1 -i $patch
done
# Similar but not the same as the other _structs.h.
rm cctools/include/foreign/mach/i386/_structs.h
cd ..

mv cctools-port/cctools/misc misc
mv cctools-port/cctools/include include
rm -r cctools-port

mkdir build
cd build

CFLAGS="-Wno-deprecated -Wno-deprecated-declarations -Wno-unused-result -Werror -Wfatal-errors -O2 -g -I../include -I../include/foreign -DPROGRAM_PREFIX=\\\"$host-\\\" -D__LITTLE_ENDIAN__ -D__private_extern__= -D__DARWIN_UNIX03"

CXXFLAGS="-std=gnu++11 $CFLAGS"

LDFLAGS="-ldl -lpthread"

for f in ../misc/libtool.c; do
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
