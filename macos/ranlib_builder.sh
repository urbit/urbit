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

mv cctools-port/cctools/misc .
mv cctools-port/cctools/include .
mv cctools-port/cctools/libstuff .
rm -r cctools-port

mkdir build
cd build

CFLAGS="-Wno-deprecated -Wno-deprecated-declarations -Wno-unused-result -Wno-format-overflow -Werror -Wfatal-errors -O2 -g -I../include -I../include/foreign -DPROGRAM_PREFIX=\\\"$host-\\\" -D__LITTLE_ENDIAN__ -D__private_extern__= -D__DARWIN_UNIX03 -DPACKAGE_NAME=\\\"cctools\\\" -DPACKAGE_VERSION=\\\"$apple_version\\\" -DEMULATED_HOST_CPU_TYPE=16777223 -DEMULATED_HOST_CPU_SUBTYPE=3"

CXXFLAGS="-std=gnu++11 $CFLAGS"

LDFLAGS="-ldl"

for f in ../libstuff/*.c ; do
  echo "compiling $f"
  eval "gcc -c $CFLAGS $f -o $(basename $f).o"
done

eval "gcc $CFLAGS ../misc/libtool.c *.o $LDFLAGS -o $host-libtool"
eval "gcc $CFLAGS -DRANLIB ../misc/libtool.c *.o $LDFLAGS -o $host-ranlib"

mkdir -p $out/bin
cp $host-libtool $host-ranlib $out/bin/

