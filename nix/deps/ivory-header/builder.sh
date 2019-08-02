source $stdenv/setup

set -ex

cleanup () {
  echo "done"
}

trap cleanup EXIT


if ! [ -f "$IVORY" ]; then
  echo "$IVORY doesn't exist"
  exit 1
fi

#
#  heuristics to confirm the ivory pill is valid
#

#  greater than 10KB
#
if [ $(du -k $IVORY | cut -f1) -gt 10 ]; then
  echo "$IVORY is less than 10KB"
fi

#  first 7 bytes != "version" (start of an lfs pointer)
#
if [ "$(cat $(IVORY) | head -c 7)" = "version" ]; then
  echo "$IVORY starts with 'version'; it's an LFS pointer"
fi

cat $IVORY > u3_Ivory.pill
xxd -i u3_Ivory.pill > ivory.h

mkdir -p $out/include

mv ivory.h $out/include
rm u3_Ivory.pill

set +x
