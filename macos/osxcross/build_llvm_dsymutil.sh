#!/usr/bin/env bash
#
# Build and install the `llvm-dsymutil` tool required for debugging.
#
# Please refer to README.DEBUGGING.md for details.
#

pushd "${0%/*}" &>/dev/null

DESC="llvm-dsymutil"
source tools/tools.sh
eval $(tools/osxcross_conf.sh)

require git
require cmake

pushd $OSXCROSS_BUILD_DIR &>/dev/null

if [ ! -e llvm-dsymutil/.clone_complete ]; then
  rm -rf llvm-dsymutil
  # Vanilla llvm-dsymutil with a few patches on top for OSXCross
  git clone https://github.com/tpoechtrager/llvm-dsymutil.git --depth 1
fi

pushd llvm-dsymutil &>/dev/null

git clean -fdx
touch .clone_complete
git pull

mkdir build
pushd build &>/dev/null

cmake .. \
  -DCMAKE_BUILD_TYPE=Release \
  -DLLVM_TARGETS_TO_BUILD="X86;ARM;AArch64" \
  -DLLVM_ENABLE_ASSERTIONS=Off

$MAKE -f tools/dsymutil/Makefile -j$JOBS
cp bin/llvm-dsymutil $OSXCROSS_TARGET_DIR/bin/osxcross-llvm-dsymutil

echo "installed llvm-dsymutil to $OSXCROSS_TARGET_DIR/bin/osxcross-llvm-dsymutil"
