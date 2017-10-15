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
#define TAPI_VERSION $version
//#define TAPI_VERSION_MAJOR 2
//#define TAPI_VERSION_MINOR 0
//#define TAPI_VERSION_PATCH 0
EOF

CFLAGS="-Iinclude -I../tapi/include -I$clang/include"

LDFLAGS="-L$clang/include"

clang-tblgen -I$clang/include -gen-clang-diags-defs \
  -o include/tapi/Driver/DiagnosticTAPIKinds.inc \
  ../tapi/include/tapi/Driver/DiagnosticTAPIKinds.td

llvm-tblgen -I$clang/include -gen-opt-parser-defs \
  -o include/tapi/Driver/TAPIOptions.inc \
  ../tapi/include/tapi/Driver/TAPIOptions.td

function build-lib() {
  name=$1
  mkdir $name
  for f in ../tapi/lib/$name/*.cpp; do
    echo "compiling $f"
    g++ -c $CFLAGS $f -o $1/$(basename $f).o
  done
  echo "archiving libtapi$1.a"
  ar cr libtapi$1.a $name/*.o
}

build-lib Config
build-lib Core
build-lib Driver
build-lib Scanner
build-lib SDKDB

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
