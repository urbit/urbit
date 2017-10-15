source $setup

tar -xf $src
mv tapi-* tapi

mkdir build
cd build

mkdir -p include/tapi/{Core,Driver}
cat > include/tapi/Core/ArchitectureConfig.h <<EOF
#pragma once
#define SUPPORT_ARCH_I386 1
#define SUPPORT_ARCH_X86_64 1
#define SUPPORT_ARCH_X86_64H 1
EOF

cat > include/tapi/Version.inc <<EOF
#pragma once
#define TAPI_VERSION 2.0.0
#define TAPI_VERSION_MAJOR 2
#define TAPI_VERSION_MINOR 0
#define TAPI_VERSION_PATCH 0
EOF

CFLAGS="-Iinclude -I../tapi/include -I$clang/include"

LDFLAGS="-L$clang/include"

clang-tblgen -I$clang/include -gen-clang-diags-defs -o include/tapi/Driver/DiagnosticTAPIKinds.inc ../tapi/include/tapi/Driver/DiagnosticTAPIKinds.td

llvm-tblgen -I$clang/include -gen-opt-parser-defs -o include/tapi/Driver/TAPIOptions.inc ../tapi/include/tapi/Driver/TAPIOptions.td

for n in ../tapi/lib/Driver/Options.cpp; do
  g++ -c $CFLAGS $n -o $(basename $n).o
done

$host-ar cr libtapi.a *.o

mkdir -p $out/lib/pkgconfig
cp *.a $out/lib

cat > $out/lib/pkgconfig/libtapi.pc <<EOF
prefix=$out
libdir=\${prefix}/lib
includedir=\${prefix}/include

Version: $version
Libs: -L\${libdir} -lopenzwave
Cflags: -I\${includedir}
EOF
