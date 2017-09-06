#!/usr/bin/env bash
#
# Build and install the GNU binutils and the GNU Debugger (gdb) for
# target OS X.
#
# You may want to run this script if you want to build software using
# gcc. Please refer to the README.md for details.
#

pushd "${0%/*}" &>/dev/null

DESC=binutils
USESYSTEMCOMPILER=1
source tools/tools.sh

eval $(tools/osxcross_conf.sh)

# binutils version to build
if [ -z "$BINUTILS_VERSION" ]; then
  BINUTILS_VERSION=2.27
fi

# gdb version to build
if [ -z "$GDB_VERSION" ]; then
  GDB_VERSION=7.11.1
fi

# mirror
MIRROR="ftp://sourceware.org/pub"

require wget

pushd $OSXCROSS_BUILD_DIR &>/dev/null

function remove_locks()
{
  rm -rf $OSXCROSS_BUILD_DIR/have_binutils*
}

function build_and_install()
{
  if [ ! -f "have_$1_$2_${OSXCROSS_TARGET}" ]; then
    pushd $OSXCROSS_TARBALL_DIR &>/dev/null
    wget -c "$MIRROR/$1/releases/$1-$2.tar.gz"
    popd &>/dev/null

    echo "cleaning up ..."
    rm -rf $1* 2>/dev/null

    extract "$OSXCROSS_TARBALL_DIR/$1-$2.tar.gz" 1

    pushd $1*$2* &>/dev/null
    mkdir -p build
    pushd build &>/dev/null

    ../configure \
      --target=x86_64-apple-$OSXCROSS_TARGET \
      --program-prefix=x86_64-apple-$OSXCROSS_TARGET- \
      --prefix=$OSXCROSS_TARGET_DIR/binutils \
      --disable-nls \
      --disable-werror

    $MAKE -j$JOBS
    $MAKE install

    popd &>/dev/null
    popd &>/dev/null
    touch "have_$1_$2_${OSXCROSS_TARGET}"
  fi
}

source $BASE_DIR/tools/trap_exit.sh

build_and_install binutils $BINUTILS_VERSION
build_and_install gdb $GDB_VERSION

echo ""
echo "installed binutils and gdb to $OSXCROSS_TARGET_DIR/binutils"
echo ""
