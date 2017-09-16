#!/usr/bin/env bash

pushd "${0%/*}" &>/dev/null

source tools/tools.sh

export TARGET

echo ""
echo "Building OSXCross toolchain, Version: $OSXCROSS_VERSION"
echo ""
echo "OS X SDK Version: $SDK_VERSION, Target: $TARGET"
echo "Minimum targeted OS X Version: $OSX_VERSION_MIN"
echo "Tarball Directory: $TARBALL_DIR"
echo "Build Directory: $BUILD_DIR"
echo "Install Directory: $TARGET_DIR"
echo "SDK Install Directory: $SDK_DIR"
echo ""

export PATH=$TARGET_DIR/bin:$PATH

mkdir -p $BUILD_DIR
mkdir -p $TARGET_DIR
mkdir -p $SDK_DIR

pushd $BUILD_DIR &>/dev/null

function remove_locks()
{
  rm -rf $BUILD_DIR/have_cctools*
}

mkdir -p $TARGET_DIR/bin

SDK=$(ls $TARBALL_DIR/MacOSX$SDK_VERSION*)

ls $TARBALL_DIR/MacOSX$SDK_VERSION*

extract $SDK 1 1

rm -rf $SDK_DIR/MacOSX$SDK_VERSION*

mv -f *OSX*$SDK_VERSION*sdk* $SDK_DIR

pushd $SDK_DIR/MacOSX$SDK_VERSION.sdk &>/dev/null
set +e
create_symlink \
  $SDK_DIR/MacOSX$SDK_VERSION.sdk/System/Library/Frameworks/Kernel.framework/Versions/A/Headers/std*.h \
  usr/include 2>/dev/null
[ ! -f "usr/include/float.h" ] && cp -f $BASE_DIR/oclang/quirks/float.h usr/include
[ $PLATFORM == "FreeBSD" ] && cp -f $BASE_DIR/oclang/quirks/tgmath.h usr/include
set -e
popd &>/dev/null

popd &>/dev/null

OSXCROSS_CONF="$TARGET_DIR/bin/osxcross-conf"
OSXCROSS_ENV="$TARGET_DIR/bin/osxcross-env"

rm -f $OSXCROSS_CONF $OSXCROSS_ENV

echo "compiling wrapper ..."

export OSXCROSS_TARGET=$TARGET
export OSXCROSS_OSX_VERSION_MIN=$OSX_VERSION_MIN
export OSXCROSS_BUILD_DIR=$BUILD_DIR

if [ "$PLATFORM" != "Darwin" ]; then
  # libLTO.so
  set +e
  eval $(cat $BUILD_DIR/cctools*/cctools/config.log | grep LLVM_LIB_DIR | head -n1)
  set -e
  export OSXCROSS_LIBLTO_PATH=$LLVM_LIB_DIR
fi

$BASE_DIR/wrapper/build.sh

echo ""

if [ $(osxcross-cmp ${SDK_VERSION/u/} "<" $OSX_VERSION_MIN) -eq 1 ]; then
  echo "OSX_VERSION_MIN must be <= SDK_VERSION"
  trap "" EXIT
  exit 1
elif [ $(osxcross-cmp $OSX_VERSION_MIN "<" 10.4) -eq 1  ]; then
  echo "OSX_VERSION_MIN must be >= 10.4"
  trap "" EXIT
  exit 1
fi

unset MACOSX_DEPLOYMENT_TARGET

test_compiler o32-clang $BASE_DIR/oclang/test.c
test_compiler o64-clang $BASE_DIR/oclang/test.c

test_compiler o32-clang++ $BASE_DIR/oclang/test.cpp
test_compiler o64-clang++ $BASE_DIR/oclang/test.cpp

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

echo ""
echo "Do not forget to add"
echo ""
echo -e "\x1B[32m${TARGET_DIR}/bin\x1B[0m"
echo ""
echo "to your PATH variable."
echo ""

echo "All done! Now you can use o32-clang(++) and o64-clang(++) like a normal compiler."
echo ""
echo "Example usage:"
echo ""
echo "Example 1: CC=o32-clang ./configure --host=i386-apple-$TARGET"
echo "Example 2: CC=i386-apple-$TARGET-clang ./configure --host=i386-apple-$TARGET"
echo "Example 3: o64-clang -Wall test.c -o test"
echo "Example 4: x86_64-apple-$TARGET-strip -x test"
echo ""
