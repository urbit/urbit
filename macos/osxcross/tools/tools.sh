#!/usr/bin/env bash

export LC_ALL="C"

BASE_DIR=$PWD

TARBALL_DIR=$BASE_DIR/tarballs
BUILD_DIR=$BASE_DIR/build
TARGET_DIR=$BASE_DIR/target
PATCH_DIR=$BASE_DIR/patches
SDK_DIR=$TARGET_DIR/SDK

PLATFORM=$(uname -s)
ARCH=$(uname -m)
SCRIPT=$(basename $0)

if [ -z "$USESYSTEMCOMPILER" ]; then
  # Default to gcc on some OSs rather than clang due to either
  # libstdc++ issues (clang uses an outdated version on those)
  # or some other incompatibilities

  case "$PLATFORM" in
    CYGWIN* | DragonFly )
      cc=gcc
      cxx=g++
    ;;
    OpenBSD )
      cc=egcc
      cxx=eg++
    ;;
    Darwin )
      cc=clang
      cxx=clang++
    ;;
    * )
      case "$ARCH" in
        arm* )
          cc=gcc
          cxx=g++
        ;;
        * )
          cc=clang
          cxx=clang++
        ;;
      esac
    ;;
  esac

  [ -z "$CC" ] && export CC=$cc
  [ -z "$CXX" ] && export CXX=$cxx
elif [ -n "$CC" -o -n "$CXX" ]; then
  echo "CC/CXX should not be set, continuing in 5 seconds..." 1>&2
  sleep 5
fi


# enable debug messages
[ -n "$OCDEBUG" ] && set -x

if [[ $SCRIPT != *wrapper/build.sh ]]; then
  # how many concurrent jobs should be used for compiling?
  if [ -z "$JOBS" ]; then
    JOBS=$(tools/get_cpu_count.sh || echo 1)
  fi

  if [ $SCRIPT != "build.sh" -a \
       $SCRIPT != "build_clang.sh" -a \
       $SCRIPT != "mount_xcode_image.sh" -a \
       $SCRIPT != "gen_sdk_package_darling_dmg.sh" -a \
       $SCRIPT != "gen_sdk_package_p7zip.sh" -a \
       $SCRIPT != "gen_cyglto_dll.sh" ]; then
    res=$(tools/osxcross_conf.sh)

    if [ $? -ne 0 ]; then
      echo -n "you must run ./build.sh first before you can start "
      echo "building $DESC"
      exit 1
    fi

    eval "$res"
  fi
fi

function require()
{
  set +e
  which $1 &>/dev/null
  while [ $? -ne 0 ]
  do
    if [ -z "$UNATTENDED" ]; then
      echo ""
      read -p "Please install '$1' then press enter"
    else
      echo "Required dependency '$1' is not installed" 1>&2
      exit 1
    fi
    which $1 &>/dev/null
  done
  set -e
}

if [[ $PLATFORM == *BSD ]] || [ $PLATFORM == "DragonFly" ]; then
  MAKE=gmake
  SED=gsed
else
  MAKE=make
  SED=sed
fi

require $SED
require $MAKE

function extract()
{
  test $# -ge 2 -a $# -lt 4 && test $2 -eq 2 && echo ""
  echo "extracting $(basename $1) ..."

  local tarflags

  tarflags="xf"
  test -n "$OCDEBUG" && tarflags+="v"

  case $1 in
    *.pkg)
      require cpio
      which xar &>/dev/null || exit 1
      xar -xf $1
      cat Payload | gunzip -dc | cpio -i 2>/dev/null && rm Payload
      ;;
    *.tar.xz)
      xz -dc $1 | tar $tarflags -
      ;;
    *.tar.gz)
      gunzip -dc $1 | tar $tarflags -
      ;;
    *.tar.bz2)
      bzip2 -dc $1 | tar $tarflags -
      ;;
    *)
      echo "Unhandled archive type" 2>&1
      exit 1
      ;;
  esac

  if [ $# -eq 2 -o $# -eq 4 ]; then
    echo ""
  fi
}

if [[ $PLATFORM == CYGWIN* ]]; then

function create_symlink()
{
  cp -f $1 $2
}

else

function create_symlink()
{
  ln -sf $1 $2
}

fi

function verbose_cmd()
{
  echo "$@"
  eval "$@"
}

function test_compiler()
{
  echo -ne "testing $1 ... "
  $1 $2 -v -O2 -Wall -o test
  rm test
  echo "works"
}

function test_compiler_cxx11()
{
  set +e
  echo -ne "testing $1 -stdlib=libc++ -std=c++11 ... "
  $1 $2 -O2 -stdlib=libc++ -std=c++11 -Wall -o test &>/dev/null
  if [ $? -eq 0 ]; then
    rm test
    echo "works"
  else
    echo "failed (ignored)"
  fi
  set -e
}

# exit on error
set -e
