#!/usr/bin/env bash
#
# Mount a Xcode .dmg (requires root) and run gen_sdk_package.sh.
#
# Works up to Xcode 4.2
#

if [ $(uname -s) != "Linux" ]; then
  echo "This script must be run on Linux"
  exit 1
fi

if [ $EUID -ne 0 ]; then
  echo "This script must be run as root"
  exit 1
fi

if [ $# -lt 1 ]; then
  echo "usage: $0 /path/to/xcode.dmg"
  exit 1
fi

case $1 in
  /*) XCODEDMG="$1" ;;
  *) XCODEDMG="$PWD/$1" ;;
esac

if [ ! -e "$XCODEDMG" ]; then
  echo "$1 does not exist"
  exit 1
fi

pushd "${0%/*}/.." &>/dev/null
source tools/tools.sh
popd &>/dev/null

require dmg2img
verbose_cmd "modprobe hfsplus"

TMP=$(mktemp -d /tmp/XXXXXXXXX)
echo "tmp dir: $TMP"

verbose_cmd "chmod 777 $TMP"

pushd $TMP &>/dev/null

PARTITION=$(dmg2img -l $XCODEDMG | grep 'disk image (Apple_HFS ' | \
            awk '{printf "%d", $2}')

case $PARTITION in
  ''|*[!0-9]*)
    echo "$XCODEDMG: cannot determine HFS partition"
    exit 1 ;;
esac

echo "HFS partition: $PARTITION"

verbose_cmd "dmg2img -p $PARTITION -i $XCODEDMG -o xcode.img &>dmg2img.log"
verbose_cmd "mkdir -m 777 mnt"
verbose_cmd "cd mnt"
verbose_cmd "mount -o loop -t hfsplus ../xcode.img $TMP/mnt"

echo ""
echo "mounted the xcode image to: $TMP/mnt"
echo ""
echo "now run (not as root):"
echo "XCODEDIR=$TMP/mnt ./tools/gen_sdk_package.sh"
echo ""
echo "once you are done with gen_sdk_package.sh, run:"
echo "umount -l $TMP/mnt && rm -rf $TMP"
echo ""

popd &>/dev/null
