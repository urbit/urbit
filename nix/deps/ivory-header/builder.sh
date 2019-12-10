source $stdenv/setup

set -e

if ! [ -f "$IVORY" ]; then
  echo "$IVORY doesn't exist"
  exit 1
fi

#
#  heuristics to confirm the ivory pill is valid
#

#  first 7 bytes != "version" (start of an lfs pointer)
#
if [ "$(head -c 7 "$IVORY")" = "version" ]; then
  echo "$IVORY is an LFS pointer (it starts with 'version')"
  echo "to fix, run: git lfs install"
  exit 1
fi

#  greater than 10KB
#
if ! [ $(du -k "$IVORY" | cut -f1) -gt 10 ]; then
  echo "$IVORY is less than 10KB"
  exit 1
fi

cat $IVORY > u3_Ivory.pill
xxd -i u3_Ivory.pill > ivory.h

mkdir -p $out/include

mv ivory.h $out/include
rm u3_Ivory.pill
