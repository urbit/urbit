#!/usr/bin/env bash
#
# Package the OS X SDKs into a tar file to be used by `build.sh`.
#

# This file comes from the osxcross project and is licensed under the GNU GPLv2.
# For more information, see the `COPYING` file from:
#  https://github.com/tpoechtrager/osxcross/tree/1a1733a773fe26e7b6c93b16fbf9341f22fac831

export LC_ALL=C

function set_xcode_dir()
{
  local tmp=$(ls $1 2>/dev/null | grep "^Xcode.*.app" | grep -v "beta" | head -n1)

  if [ -z "$tmp" ]; then
    tmp=$(ls $1 2>/dev/null | grep "^Xcode.*.app" | head -n1)
  fi

  if [ -n "$tmp" ]; then
    XCODEDIR="$1/$tmp"
  fi
}

if [ $(uname -s) != "Darwin" ]; then
  if [ -z "$XCODEDIR" ]; then
    echo "This script must be run on OS X" 1>&2
    echo "... Or with XCODEDIR=... on Linux" 1>&2
    exit 1
  else
    case $XCODEDIR in
      /*) ;;
      *) XCODEDIR="$PWD/$XCODEDIR" ;;
    esac
    set_xcode_dir $XCODEDIR
  fi
else
  set_xcode_dir $(echo /Volumes/Xcode* | tr ' ' '\n' | grep -v "beta" | head -n1)

  if [ -z "$XCODEDIR" ]; then
    set_xcode_dir /Applications

    if [ -z "$XCODEDIR" ]; then
      set_xcode_dir $(echo /Volumes/Xcode* | tr ' ' '\n' | head -n1)

      if [ -z "$XCODEDIR" ]; then
        echo "please mount Xcode.dmg" 1>&2
        exit 1
      fi
    fi
  fi
fi

if [ ! -d $XCODEDIR ]; then
  echo "cannot find Xcode (XCODEDIR=$XCODEDIR)" 1>&2
  exit 1
fi

echo -e "found Xcode: $XCODEDIR"

WDIR=$(pwd)

which gnutar &>/dev/null

if [ $? -eq 0 ]; then
  TAR=gnutar
else
  TAR=tar
fi

which xz &>/dev/null

if [ $? -eq 0 ]; then
  COMPRESSOR=xz
  PKGEXT="tar.xz"
else
  COMPRESSOR=bzip2
  PKGEXT="tar.bz2"
fi

set -e

pushd $XCODEDIR &>/dev/null

if [ -d "Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs" ]; then
  pushd "Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs" &>/dev/null
else
  if [ -d "../Packages" ]; then
    pushd "../Packages" &>/dev/null
  elif [ -d "Packages" ]; then
    pushd "Packages" &>/dev/null
  else
    if [ $? -ne 0 ]; then
      echo "Xcode (or this script) is out of date" 1>&2
      echo "trying some magic to find the SDKs anyway ..." 1>&2

      SDKDIR=$(find . -name SDKs -type d | grep MacOSX | head -n1)

      if [ -z "$SDKDIR" ]; then
        echo "cannot find SDKs!" 1>&2
        exit 1
      fi

      pushd $SDKDIR &>/dev/null
    fi
  fi
fi

SDKS=$(ls | grep "^MacOSX10.*" | grep -v "Patch")

if [ -z "$SDKS" ]; then
    echo "No SDK found" 1>&2
    exit 1
fi

# Xcode 5
LIBCXXDIR1="Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/c++/v1"

# Xcode 6
LIBCXXDIR2="Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include/c++/v1"

# Manual directory
MANDIR="Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/share/man"

for SDK in $SDKS; do
  echo -n "packaging $(echo "$SDK" | sed -E "s/(.sdk|.pkg)//g") SDK "
  echo "(this may take several minutes) ..."

  if [[ $SDK == *.pkg ]]; then
    cp $SDK $WDIR
    continue
  fi

  TMP=$(mktemp -d /tmp/XXXXXXXXXXX)
  cp -r $SDK $TMP &>/dev/null || true

  pushd $XCODEDIR &>/dev/null

  # libc++ headers for C++11/C++14
  if [ -d $LIBCXXDIR1 ]; then
    cp -rf $LIBCXXDIR1 "$TMP/$SDK/usr/include/c++"
  elif [ -d $LIBCXXDIR2 ]; then
    cp -rf $LIBCXXDIR2 "$TMP/$SDK/usr/include/c++"
  fi

  if [ -d $MANDIR ]; then
    mkdir -p $TMP/$SDK/usr/share/man
    cp -rf $MANDIR/* $TMP/$SDK/usr/share/man
  fi

  popd &>/dev/null

  pushd $TMP &>/dev/null
  $TAR -cf - * | $COMPRESSOR -9 -c - > "$WDIR/$SDK.$PKGEXT"
  popd &>/dev/null

  rm -rf $TMP
done

popd &>/dev/null
popd &>/dev/null

echo ""
ls -lh | grep MacOSX
