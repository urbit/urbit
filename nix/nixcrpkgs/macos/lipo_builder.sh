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

# Causes a troublesome undefined reference.
rm cctools/libstuff/vm_flush_cache.c

cd ..

mv cctools-port/cctools/misc/lipo.c .
mv cctools-port/cctools/include .
mv cctools-port/cctools/libstuff .
rm -r cctools-port

mkdir build
cd build

CFLAGS="-Wno-deprecated -Wno-deprecated-declarations -Wno-unused-result -Werror -Wfatal-errors -O2 -g -I../include -I../include/foreign -DPROGRAM_PREFIX=\\\"$host-\\\" -D__LITTLE_ENDIAN__ -D__private_extern__= -D__DARWIN_UNIX03 -DPACKAGE_NAME=\\\"cctools\\\" -DPACKAGE_VERSION=\\\"$apple_version\\\" -DEMULATED_HOST_CPU_TYPE=16777223 -DEMULATED_HOST_CPU_SUBTYPE=3"

CXXFLAGS="-std=gnu++11 $CFLAGS"

LDFLAGS="-ldl -lpthread"

for f in ../lipo.c ../libstuff/*.c; do
  echo "compiling $f"
  eval "gcc -c $CFLAGS $f -o $(basename $f).o"
done

gcc *.o $LDFLAGS -o lipo

mkdir -p $out/bin
cp lipo $out/bin/
