#!/usr/bin/env bash
#
# Extract required files from a Xcode .dmg using p7zip and run
# gen_sdk_package.sh.
#
# Works up to Xcode 7.2
#
# p7zip will be downloaded and compiled if missing.
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

case $1 in
  /*) XCODEDMG="$1" ;;
  *) XCODEDMG="$PWD/$1" ;;
esac

mkdir -p $BUILD_DIR

require git
require $MAKE

[ -n "$CC" ] && require $CC
[ -n "$CXX" ] && require $CXX

pushd $BUILD_DIR &>/dev/null

if [ ! -f $TARGET_DIR/SDK/tools/bin/7z ]; then
  rm -f have_p7zip
fi

if [ ! -f "have_p7zip" ]; then

rm -rf p7zip*
git clone https://github.com/tpoechtrager/p7zip.git
pushd p7zip &>/dev/null
if [ -n "$CC" ] && [ -n "$CXX" ]; then
  [[ $CC == *clang* ]] && CC="$CC -Qunused-arguments"
  [[ $CXX == *clang* ]] && CXX="$CXX -Qunused-arguments"
  $MAKE 7z -j $JOBS CC="$CC" CXX="$CXX"
else
  $MAKE 7z -j $JOBS
fi
$MAKE install DEST_HOME=$TARGET_DIR/SDK/tools
find $TARGET_DIR/SDK/tools/share -type f -exec chmod 0664 {} \;
find $TARGET_DIR/SDK/tools/share -type d -exec chmod 0775 {} \;
popd &>/dev/null

touch "have_p7zip"

fi

popd &>/dev/null

#/tmp is prone to run out of space
#TMP=$(mktemp -d /tmp/XXXXXXXXX)

for i in {1..100}; do
  TMP="tmp_$RANDOM"
  [ -e $TMP ] && continue
  mkdir $TMP && break
done

if [ ! -d $TMP ]; then
  echo "cannot create $PWD/$TMP directory" 1>&2
  exit 1
fi

function cleanup() {
  popd &>/dev/null || true
  rm -rf $TMP
}

trap cleanup EXIT

pushd $TMP &>/dev/null

set +e

$TARGET_DIR/SDK/tools/bin/7z x \
  $XCODEDMG \
  "*/Xcode*.app/Contents/Developer/Platforms/MacOSX.platform" \
  "*/Xcode*.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain"

[ $? -ne 0 -a $? -ne 2 ] && exit 1

if [ -z "$(ls -A)" ]; then
  $TARGET_DIR/SDK/tools/bin/7z x $XCODEDMG "*/Packages/MacOSX*.pkg"
  [ $? -ne 0 -a $? -ne 2 ] && exit 1
fi

[ -z "$(ls -A)" ] && exit 1

set -e

popd &>/dev/null

XCODEDIR="$TMP/$(ls $TMP | grep "code" | head -n1)" \
  ./tools/gen_sdk_package.sh
