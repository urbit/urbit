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

mkdir include
cat > include/configure__.h <<EOF
#pragma once

#include <sys/param.h>
#include <limits.h>
#include <unistd.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <dlfcn.h>
#include <algorithm>
#include "strlcat.h"
#include "strlcpy.h"
#include "helper.h"

#define CPU_SUBTYPE_X86_ALL     ((cpu_subtype_t)3)

#define SUPPORT_ARCH_i386 1
#define SUPPORT_ARCH_x86_64 1
#define SUPPORT_ARCH_arm64 1
#define ALL_SUPPORTED_ARCHS  "i386 x86_64 arm64"

#define BITCODE_XAR_VERSION "1.0"

#ifndef HW_NCPU
//#define HW_NCPU 3
#endif

#ifndef CTL_HW
//#define CTL_HW  6
#endif

#ifndef ARG_MAX
//#define ARG_MAX 31072
#endif

#define PROGRAM_PREFIX "${host}-"
EOF

for f in ../ld64/src/ld/*.c ../ld64/src/3rd/*.c; do
  echo "compiling $f"
  eval "gcc -c $CFLAGS $f -o $(basename $f).o"
done

for f in $(find ../ld64/src -name \*.cpp); do
  echo "compiling $f"
  eval "g++ -c $CFLAGS $f -o $(basename $f).o"
done

g++ *.o -ldl -lpthread -o $host-ld

mkdir -p $out/bin
cp $host-ld $out/bin
