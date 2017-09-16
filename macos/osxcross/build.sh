#!/usr/bin/env bash

pushd "${0%/*}" &>/dev/null

source tools/tools.sh

mkdir -p $BUILD_DIR
mkdir -p $TARGET_DIR
mkdir -p $SDK_DIR

pushd $BUILD_DIR &>/dev/null

mkdir -p $TARGET_DIR/bin

export OSXCROSS_BUILD_DIR=$BUILD_DIR

# NOTE: osxcross set this to LLVM_LIB_DIR from cctools config.log,
# so maybe we should do the same
export OSXCROSS_LIBLTO_PATH=

cd $BASE_DIR/wrapper

set -x

if [[ $PLATFORM == CYGWIN* ]]; then
  EXESUFFIX=".exe"
else
  EXESUFFIX=""
fi

function create_wrapper_link
{
  # arg 1:
  #  program name
  # arg 2:
  #  1: create a standalone link and links with the target triple prefix
  #  2: create links with target triple prefix and shortcut links such
  #     as o32, o64, ...
  #
  # example:
  #  create_wrapper_link osxcross 1
  # creates the following symlinks:
  #  -> osxcross
  #  -> i386-apple-darwinXX-osxcross
  #  -> x86_64-apple-darwinXX-osxcross
  #  -> x86_64h-apple-darwinXX-osxcross

  if [ $# -ge 2 ] && [ $2 -eq 1 ]; then
    verbose_cmd create_symlink "${TARGETTRIPLE}-wrapper${EXESUFFIX}" \
      "${1}${EXESUFFIX}"
  fi

  verbose_cmd create_symlink "${TARGETTRIPLE}-wrapper${EXESUFFIX}" \
    "i386-apple-${OSXCROSS_TARGET}-${1}${EXESUFFIX}"

  verbose_cmd create_symlink "${TARGETTRIPLE}-wrapper${EXESUFFIX}" \
    "x86_64-apple-${OSXCROSS_TARGET}-${1}${EXESUFFIX}"

  if [ -n "$X86_64H_SUPPORTED" ] && [ $X86_64H_SUPPORTED -eq 1 ] &&
     ([[ $1 != gcc* ]] && [[ $1 != g++* ]] && [[ $1 != *gstdc++ ]]); then
    verbose_cmd create_symlink "${TARGETTRIPLE}-wrapper${EXESUFFIX}" \
      "x86_64h-apple-${OSXCROSS_TARGET}-${1}${EXESUFFIX}"
  fi

  if [ $# -ge 2 ] && [ $2 -eq 2 ]; then
    verbose_cmd create_symlink "${TARGETTRIPLE}-wrapper${EXESUFFIX}" \
      "o32-${1}${EXESUFFIX}"
    verbose_cmd create_symlink "${TARGETTRIPLE}-wrapper${EXESUFFIX}" \
      "o64-${1}${EXESUFFIX}"

    if [ -n "$X86_64H_SUPPORTED" ] && [ $X86_64H_SUPPORTED -eq 1 ] &&
       ([[ $1 != gcc* ]] && [[ $1 != g++* ]] && [[ $1 != *gstdc++ ]]); then
      verbose_cmd create_symlink "${TARGETTRIPLE}-wrapper${EXESUFFIX}" \
        "o64h-${1}${EXESUFFIX}"
    fi
  fi
}

[ -z "$TARGETCOMPILER" ] && TARGETCOMPILER=clang

TARGETTRIPLE=x86_64-apple-${OSXCROSS_TARGET}

FLAGS=""

if [ -n "$BWPLATFORM" ]; then
  PLATFORM=$BWPLATFORM

  if [ $PLATFORM = "Darwin" -a $(uname -s) != "Darwin" ]; then
    CXX=$(xcrun -f clang++)
    #CXX=$(xcrun -f g++)
    FLAGS+="-fvisibility-inlines-hidden "
  elif [ $PLATFORM = "FreeBSD" -a $(uname -s) != "FreeBSD" ]; then
    CXX=amd64-pc-freebsd10.1-clang++
    #CXX=amd64-pc-freebsd10.1-g++
  elif [ $PLATFORM = "NetBSD" -a $(uname -s) != "NetBSD" ]; then
    CXX=amd64-pc-netbsd6.1.3-clang++
    #CXX=amd64-pc-netbsd6.1.3-g++
  fi

  [ -z "$BWCOMPILEONLY" ] && BWCOMPILEONLY=1
else
  [ -z "$PORTABLE" ] && FLAGS="$CXXFLAGS "
fi

if [ -n "$BWCXX" ]; then
  [ "$CXX" != "$BWCXX" ] && echo "using $BWCXX" 1>&2
  CXX=$BWCXX
fi

if [ "$PLATFORM" == "Linux" ]; then
  FLAGS+="-isystem quirks/include "
fi

mkdir -p ../target ../target/bin
export PLATFORM
export CXX

verbose_cmd $MAKE clean

OSXCROSS_CXXFLAGS="$FLAGS" \
  verbose_cmd $MAKE wrapper -j$JOBS

mv wrapper $TARGET_DIR/bin/$host-wrapper


