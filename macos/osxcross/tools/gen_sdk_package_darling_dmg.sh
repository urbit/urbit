#!/usr/bin/env bash
#
# Mount a Xcode .dmg (using fuse) and run gen_sdk_package.sh.
#
# Works up to Xcode 7.3
#
# This script uses darling-dmg and fuse to mount the .dmg, thus
# avoiding to actually unpack it.
# darling-dmg will be downloaded and compiled if missing.
#

pushd "${0%/*}/.." &>/dev/null
source tools/tools.sh

if [ $PLATFORM == "Darwin" ]; then
  echo "Use gen_sdk_package.sh on Mac OS X" 1>&2
  exit 1
fi

if [ $# -eq 0 ]; then
  echo "Usage: $0 <xcode.dmg>" 1>&2
  exit 1
fi

mkdir -p $BUILD_DIR

require git
require cmake
require $MAKE
require modinfo
require fusermount

[ -n "$CC" ] && require $CC
[ -n "$CXX" ] && require $CXX

set +e

command -v lsb_release 2>&1 > /dev/null

if [[ $? -eq 0 ]] && [[ -n $(lsb_release -a 2>&1 | grep -i ubuntu) ]]; then
  echo "Using ubuntu, skipping fuse module check"
else
  modinfo fuse &>/dev/null
fi

if [ $? -ne 0 ]; then
  echo "Required kernel module 'fuse' not loaded" 1>&2
  echo "Please run 'insmod fuse' as root" 1>&2
  exit 1
fi

set -e

pushd $BUILD_DIR &>/dev/null

if [ ! -f $TARGET_DIR/SDK/tools/bin/darling-dmg ]; then
  rm -f have_darling_dmg
fi

DARLING_DMG_REV="b7ce87bfe59c2ed758165c8650402f6d4c84d184"

if [ ! -f "have_darling_dmg_$DARLING_DMG_REV" ]; then

rm -rf darling-dmg*
git clone https://github.com/LubosD/darling-dmg.git
pushd darling-dmg &>/dev/null
git reset --hard $DARLING_DMG_REV
mkdir -p build
pushd build &>/dev/null
cmake .. -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=$TARGET_DIR/SDK/tools
$MAKE -j $JOBS install
popd &>/dev/null
popd &>/dev/null

touch "have_darling_dmg_$DARLING_DMG_REV"

fi

popd &>/dev/null # build dir

TMP=$(mktemp -d /tmp/XXXXXXXXX)

function cleanup() {
  fusermount -u $TMP || true
  rm -rf $TMP
}

trap cleanup EXIT

LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$TARGET_DIR/SDK/tools/lib \
  $TARGET_DIR/SDK/tools/bin/darling-dmg $1 $TMP

XCODEDIR=$TMP ./tools/gen_sdk_package.sh
