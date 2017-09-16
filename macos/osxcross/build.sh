#!/usr/bin/env bash

pushd "${0%/*}" &>/dev/null

source tools/tools.sh

echo "Building OSXCross toolchain, Version: $OSXCROSS_VERSION"
echo "OS X SDK Version: $SDK_VERSION, Target: $TARGET"
echo "Minimum targeted OS X Version: $OSX_VERSION_MIN"
echo "Tarball Directory: $TARBALL_DIR"
echo "Build Directory: $BUILD_DIR"
echo "Install Directory: $TARGET_DIR"
echo "SDK Install Directory: $SDK_DIR"

export PATH=$TARGET_DIR/bin:$PATH

mkdir -p $BUILD_DIR
mkdir -p $TARGET_DIR
mkdir -p $SDK_DIR

pushd $BUILD_DIR &>/dev/null

mkdir -p $TARGET_DIR/bin

export OSXCROSS_TARGET=$TARGET
export OSXCROSS_OSX_VERSION_MIN=$OSX_VERSION_MIN
export OSXCROSS_BUILD_DIR=$BUILD_DIR

# NOTE: osxcross set this to LLVM_LIB_DIR from cctools config.log,
# so maybe we should do the same
export OSXCROSS_LIBLTO_PATH=

$BASE_DIR/wrapper/build.sh

cat > test1.c <<EOF
#include <stdio.h>
int main() { printf("hello\n"); }
EOF
o64-clang -c test1.c -o test1.o
o64-clang -v -v -v -v test1.o -o test1

test_compiler o64-clang $BASE_DIR/oclang/test.c
test_compiler o64-clang++ $BASE_DIR/oclang/test.cpp

# NOTE: o32-clang doesn't work currently because we don't have an i386 ld on the path.
# Probably we don't care and should just remove o32-clang.
test_compiler o32-clang $BASE_DIR/oclang/test.c
test_compiler o32-clang++ $BASE_DIR/oclang/test.cpp

if [ $(osxcross-cmp ${SDK_VERSION/u/} ">=" 10.7) -eq 1 ]; then
  if [ ! -d "$SDK_DIR/MacOSX$SDK_VERSION.sdk/usr/include/c++/v1" ]; then
    echo ""
    echo -n "Given SDK does not contain libc++ headers "
    echo "(-stdlib=libc++ test may fail)"
    echo -n "You may want to re-package your SDK using "
    echo "'tools/gen_sdk_package.sh' on OS X"
  fi
  echo ""
  test_compiler_cxx11 o32-clang++ $BASE_DIR/oclang/test_libcxx.cpp
  test_compiler_cxx11 o64-clang++ $BASE_DIR/oclang/test_libcxx.cpp
fi
