#!/usr/bin/env bash
#
# Build and install the LTO library, which the Cygwin package of
# libLLVM is lacking.
#

pushd "${0%/*}/.." &>/dev/null
source tools/tools.sh

if [[ $PLATFORM != CYGWIN* ]]; then
  exit 1
fi

LLVM_CONFIG="llvm-config"

CXXFLAGS="$($LLVM_CONFIG --cxxflags) -fno-PIC"
LDFLAGS="$($LLVM_CONFIG --cxxflags) -Wl,-s"
INCDIR=$($LLVM_CONFIG --includedir)
LIBDIR=$($LLVM_CONFIG --libdir)
LIBS=$($LLVM_CONFIG --libs all)
SYSLIBS="$($LLVM_CONFIG --system-libs) -ledit -lffi"

VERSION=$($LLVM_CONFIG --version | awk -F \. {'print $1$2'} | sed 's/svn//g')

set -e
TMP=$(mktemp -d)
set +e

pushd $TMP &>/dev/null
wget https://raw.githubusercontent.com/llvm-mirror/llvm/release_$VERSION/tools/lto/lto.cpp
wget https://raw.githubusercontent.com/llvm-mirror/llvm/release_$VERSION/tools/lto/LTODisassembler.cpp
wget https://raw.githubusercontent.com/llvm-mirror/llvm/release_$VERSION/tools/lto/lto.exports

echo "{"               > cyglto.exports
echo "  global:"      >> cyglto.exports
while read p; do
  echo "   $p;"       >> cyglto.exports
done < lto.exports
echo "   LLVM*;"      >> cyglto.exports
echo "  local: *;"    >> cyglto.exports
echo "};"             >> cyglto.exports

if [ $ARCH == "x86_64" ]; then
  # https://github.com/tpoechtrager/osxcross/issues/91
  mkdir -p llvm/LTO
  echo "#undef off_t"                               > llvm/LTO/LTOModule.h
  echo "#define off_t long long"                   >> llvm/LTO/LTOModule.h
  echo "#include_next \"llvm/LTO/LTOModule.h\""    >> llvm/LTO/LTOModule.h
  CXXFLAGS="-I $TMP $CXXFLAGS"
fi

popd &>/dev/null

set -x

g++ -shared \
 -L$LIBDIR -I$INCDIR $CXXFLAGS $LDFLAGS \
 $TMP/lto.cpp $TMP/LTODisassembler.cpp -Wl,-version-script,$TMP/cyglto.exports \
 -Wl,--whole-archive $LIBS -Wl,--no-whole-archive $SYSLIBS \
 -o /bin/cygLTO.dll -Wl,--out-implib,/lib/libLTO.dll.a

rm -rf $TMP

popd &>/dev/null
