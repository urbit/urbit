#!/usr/bin/env bash
#
# Build and install the "compiler-rt" runtime library.
#
# This requires that you already finished `build.sh`.
# Please refer to README.COMPILER-RT.md for details.
#

pushd "${0%/*}" &>/dev/null

DESC=compiler-rt
source tools/tools.sh
eval $(tools/osxcross_conf.sh)

if [ $PLATFORM == "Darwin" ]; then
  exit 1
fi

require git

CLANG_VERSION=$(echo "__clang_major__ __clang_minor__ __clang_patchlevel__" | \
 xcrun clang -xc -E - | tail -n1 | tr ' ' '.')

# Drop patch level for <= 3.3.
if [ $(osxcross-cmp $CLANG_VERSION "<=" 3.3) -eq 1 ]; then
  CLANG_VERSION=$(echo $CLANG_VERSION | tr '.' ' ' |
                  awk '{print $1, $2}' | tr ' ' '.')
fi

CLANG_LIB_DIR=$(clang -print-search-dirs | grep "libraries: =" | \
                tr '=' ' ' | tr ':' ' ' | awk '{print $2}')

VERSION=$(echo "${CLANG_LIB_DIR}" | tr '/' '\n' | tail -n1)

if [ $VERSION != $CLANG_VERSION ]; then
  echo "sanity check failed: $VERSION != ${CLANG_VERSION}" 1>&2
  exit 1
fi

CLANG_INCLUDE_DIR="${CLANG_LIB_DIR}/include"
CLANG_DARWIN_LIB_DIR="${CLANG_LIB_DIR}/lib/darwin"

USE_CMAKE=0

case $CLANG_VERSION in
  3.2*) BRANCH=release_32 ;;
  3.3*) BRANCH=release_33 ;;
  3.4*) BRANCH=release_34 ;;
  3.5*) BRANCH=release_35 ;;
  3.6*) BRANCH=release_36 ;;
  3.7*) BRANCH=release_37 ;;
  3.8*) BRANCH=release_38; USE_CMAKE=1; ;;
  3.9*) BRANCH=release_39; USE_CMAKE=1; ;;
  4.0*) BRANCH=release_40; USE_CMAKE=1; ;;
  5.0*) BRANCH=release_50; USE_CMAKE=1; ;;
  6.0*) BRANCH=master; USE_CMAKE=1; ;;
  * ) echo "Unsupported Clang version, must be >= 3.2 and <= 6.0" 1>&2; exit 1;
esac

if [ $(osxcross-cmp $CLANG_VERSION ">=" 3.5) -eq 1 ]; then
  export MACOSX_DEPLOYMENT_TARGET=10.8 # x86_64h
else
  export MACOSX_DEPLOYMENT_TARGET=10.4
fi

if [ $(osxcross-cmp $MACOSX_DEPLOYMENT_TARGET ">" \
                    $OSXCROSS_SDK_VERSION) -eq 1 ];
then
  echo ">= $MACOSX_DEPLOYMENT_TARGET SDK required" 1>&2
  exit 1
fi

pushd $OSXCROSS_BUILD_DIR &>/dev/null

if [ ! -e compiler-rt/.clone_complete ]; then
  rm -rf compiler-rt
  git clone http://llvm.org/git/compiler-rt.git
fi

pushd compiler-rt &>/dev/null

git reset --hard
git clean -fdx
git checkout $BRANCH
touch .clone_complete
git pull

EXTRA_MAKE_FLAGS=""
if [ -n "$OCDEBUG" ]; then
  EXTRA_MAKE_FLAGS+="VERBOSE=1 "
fi

export OSXCROSS_NO_X86_64H_DEPLOYMENT_TARGET_WARNING=1


if [ $USE_CMAKE -eq 1 ]; then

  ### CMAKE ###

  require cmake

  $SED -i 's/COMMAND xcodebuild -version -sdk ${sdk_name}.internal Path/'\
\ \ \ \ \ \ 'COMMAND xcrun -sdk ${sdk_name}.internal --show-sdk-path/g' \
    cmake/Modules/CompilerRTDarwinUtils.cmake

  $SED -i 's/COMMAND xcodebuild -version -sdk ${sdk_name} Path/'\
\ \ \ \ \ \ 'COMMAND xcrun -sdk ${sdk_name} --show-sdk-path/g' \
    cmake/Modules/CompilerRTDarwinUtils.cmake

  $SED -i "s/COMMAND lipo /COMMAND xcrun lipo /g" \
    cmake/Modules/CompilerRTDarwinUtils.cmake

  $SED -i "s/COMMAND ld /COMMAND xcrun ld /g" \
    cmake/Modules/CompilerRTDarwinUtils.cmake

  $SED -i "s/COMMAND codesign /COMMAND true /g" \
    cmake/Modules/AddCompilerRT.cmake

  mkdir build
  pushd build &>/dev/null

  CC=$(xcrun -f clang) CXX=$(xcrun -f clang++) cmake .. \
    -DCMAKE_BUILD_TYPE=Release -DCMAKE_SYSTEM_NAME=Darwin \
    -DCMAKE_OSX_SYSROOT=$(xcrun --show-sdk-path) -DCMAKE_AR=$(xcrun -f ar)

  $MAKE -j $JOBS $EXTRA_MAKE_FLAGS

  popd &>/dev/null

  ### CMAKE END ###

else

  ### MAKE ###

  $SED -i "s/Configs += ios//g" make/platform/clang_darwin.mk
  $SED -i "s/Configs += cc_kext_ios5//g" make/platform/clang_darwin.mk
  $SED -i "s/Configs += profile_ios//g" make/platform/clang_darwin.mk
  $SED -i "s/Configs += asan_iossim_dynamic//g" make/platform/clang_darwin.mk

  # Unbreak the -Werror build.
  if [ -f lib/asan/asan_mac.h ]; then
    $SED -i "s/ASAN__MAC_H/ASAN_MAC_H/g" lib/asan/asan_mac.h
  fi

  EXTRA_MAKE_FLAGS+="LIPO=\"$(xcrun -f lipo)\""

  if [ $(osxcross-cmp $CLANG_VERSION "<=" 3.3) -eq 1 ]; then
    EXTRA_MAKE_FLAGS+=" AR=\"$(xcrun -f ar)\""
    EXTRA_MAKE_FLAGS+=" RANLIB=\"$(xcrun -f ranlib)\""
    EXTRA_MAKE_FLAGS+=" CC=\"$(xcrun -f clang)\""
  fi

  # Must eval here because of the spaces in EXTRA_MAKE_FLAGS.

  eval "$MAKE clang_darwin $EXTRA_MAKE_FLAGS -j $JOBS"

  ### MAKE END ###

fi


echo ""
echo ""
echo ""
echo "Please run the following commands by hand to install compiler-rt:"
echo ""

echo "mkdir -p ${CLANG_INCLUDE_DIR}"
echo "mkdir -p ${CLANG_DARWIN_LIB_DIR}"
echo "cp -rv $PWD/include/sanitizer ${CLANG_INCLUDE_DIR}"



if [ $USE_CMAKE -eq 1 ]; then

  ### CMAKE ###

  echo "cp -v $PWD/build/lib/darwin/*.a ${CLANG_DARWIN_LIB_DIR}"
  echo "cp -v $PWD/build/lib/darwin/*.dylib ${CLANG_DARWIN_LIB_DIR}"

  ### CMAKE END ###

else

  ### MAKE ###

  pushd "clang_darwin" &>/dev/null

  function print_install_command() {
    if [ -f "$1" ]; then
      echo "cp $PWD/$1 ${CLANG_DARWIN_LIB_DIR}/$2"
    fi
  }

  print_install_command "osx/libcompiler_rt.a"         "libclang_rt.osx.a"
  print_install_command "10.4/libcompiler_rt.a"        "libclang_rt.10.4.a"
  print_install_command "eprintf/libcompiler_rt.a"     "libclang_rt.eprintf.a"
  print_install_command "cc_kext/libcompiler_rt.a"     "libclang_rt.cc_kext.a"
  print_install_command "profile_osx/libcompiler_rt.a" "libclang_rt.profile_osx.a"


  print_install_command "ubsan_osx_dynamic/libcompiler_rt.dylib" \
    "libclang_rt.ubsan_osx_dynamic.dylib"

  print_install_command "asan_osx_dynamic/libcompiler_rt.dylib" \
    "libclang_rt.asan_osx_dynamic.dylib"


  popd &>/dev/null

  ### MAKE END ###

fi


echo ""

