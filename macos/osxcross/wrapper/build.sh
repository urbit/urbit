#!/usr/bin/env bash

set -x

pushd "${0%/*}" &>/dev/null
pushd .. &>/dev/null
source ./tools/tools.sh
popd &>/dev/null

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

function compile_wrapper()
{
  mkdir -p ../target ../target/bin
  export PLATFORM
  export CXX

  verbose_cmd $MAKE clean

  OSXCROSS_CXXFLAGS="$FLAGS" \
    verbose_cmd $MAKE wrapper -j$JOBS
}

compile_wrapper

if [ -n "$BWCOMPILEONLY" ]; then
  exit 0
fi

verbose_cmd mv wrapper "${TARGET_DIR}/bin/${TARGETTRIPLE}-wrapper${EXESUFFIX}"

pushd "../target/bin" &>/dev/null

if [ $TARGETCOMPILER = "clang" ]; then
  create_wrapper_link clang 2
  create_wrapper_link clang++ 2
  create_wrapper_link clang++-libc++ 2
  create_wrapper_link clang++-stdc++ 2
  create_wrapper_link clang++-gstdc++ 2
elif [ $TARGETCOMPILER = "gcc" ]; then
  create_wrapper_link gcc 2
  create_wrapper_link g++ 2
  create_wrapper_link g++-libc++ 2
fi

create_wrapper_link cc
create_wrapper_link c++

create_wrapper_link osxcross 1
create_wrapper_link osxcross-conf 1
create_wrapper_link osxcross-env 1
create_wrapper_link osxcross-cmp 1
create_wrapper_link osxcross-man 1
create_wrapper_link pkg-config

if [ "$PLATFORM" != "Darwin" ]; then
  create_wrapper_link sw_vers 1
  create_wrapper_link dsymutil 1
  create_wrapper_link xcrun 1
fi

popd &>/dev/null
popd &>/dev/null
