source $setup

tar -xf $src
mv cctools-port-* cctools-port

cd cctools-port
for patch in $patches; do
  echo applying patch $patch
  patch -p1 -i $patch
done
cd ..

mv cctools-port/cctools/ld64 ld64
mv cctools-port/cctools/include include
rm -r cctools-port

mkdir build
cd build

mkdir include
cat > include/configure.h <<EOF
#ifndef _CONFIGURE_H
#define _CONFIGURE_H
#include <sys/param.h>
#include <limits.h>
#include <unistd.h>
#include <stddef.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <dlfcn.h>

#include "strlcat.h"
#include "strlcpy.h"
#include "helper.h"

#ifdef __GLIBCXX__
//#include <algorithm>
#endif

//#define CPU_SUBTYPE_X86_ALL     ((cpu_subtype_t)3)

#define SUPPORT_ARCH_x86_64 1
#define ALL_SUPPORTED_ARCHS  "x86_64"

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

#endif
EOF

for f in ../ld64/src/ld/Options.cpp; do
  echo "compiling $f"
  gcc -c $CXXFLAGS $f -o $(basename $f).o
done
