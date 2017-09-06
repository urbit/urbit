#!/usr/bin/env bash

set -ex

test -z "$COMPRESSLEVEL" && COMPRESSLEVEL=9

if [ -n "$BINARYPACKAGE" ]; then
  SUFFIX=""
else
  SUFFIX="_src"
  BINARYPACKAGE="0"
fi

TMPDIR=`mktemp -d /tmp/XXXXXXXXX`

BASEDIR=`pwd`

set +e
REVHASH=`git rev-parse --short HEAD`
set -e

OSXCROSSVER=`cat build.sh | grep "OSXCROSS_VERSION" | head -n1 | tr '=' ' ' | awk '{print $2}'`

pushd $TMPDIR

mkdir osxcross
pushd osxcross

if [ $BINARYPACKAGE != "1" ]; then
  cp -r $BASEDIR/tarballs .
  cp -r $BASEDIR/patches .
  cp -r $BASEDIR/tools .
  cp -r $BASEDIR/oclang .
  cp -r $BASEDIR/wrapper .
else
  ldd `ls $BASEDIR/target/bin/x86_64-apple-darwin*-ld | head -n1` | grep "libLTO.so" &>/dev/null && \
    echo "-->> WARNING: ld is linked dynamically against libLTO.so! Consider recompiling with DISABLE_LTO_SUPPORT=1 <<--" && \
    sleep 5

  cp -r $BASEDIR/target/* .
  cp $BASEDIR/build/cctools*/cctools/APPLE_LICENSE CCTOOLS.LICENSE
  cp $BASEDIR/oclang/find_intrinsic_headers.sh bin/osxcross-fix-intrinsic-headers

  READMEINSTALL="README_INSTALL"

  echo "- BINARY INSTALLATION INSTRUCTIONS -"     > $READMEINSTALL
  echo ""                                        >> $READMEINSTALL
  echo "Add "                                    >> $READMEINSTALL
  echo ""                                        >> $READMEINSTALL
  echo "  \`<absolute path>/bin/osxcross-env\`"  >> $READMEINSTALL
  echo ""                                        >> $READMEINSTALL
  echo "To your ~/.profile or ~/.bashrc,"        >> $READMEINSTALL
  echo "then restart your shell session."        >> $READMEINSTALL
  echo ""                                        >> $READMEINSTALL
  echo "That's it."                              >> $READMEINSTALL
  echo ""                                        >> $READMEINSTALL
fi

find $BASEDIR -maxdepth 1 -type f -exec cp {} . \;

if [ $BINARYPACKAGE == "1" ]; then
  rm -f *.sh
  rm -f TODO
fi

rm -rf tarballs/old*
rm -rf tarballs/gcc*
rm -rf tarballs/MacOSX*

rm -f tools/cpucount

rm -f osxcross*.tar.*

find . \( -name "*.save" -o -name "*~" -o -name "*.kate-swp" \) -exec rm {} \;

rm -rf osxcross*.tar.*

popd

tar -cf - * | xz -$COMPRESSLEVEL -c - > $BASEDIR/osxcross-v${OSXCROSSVER}_${REVHASH}${SUFFIX}.tar.xz

popd

rm -rf $TMPDIR
