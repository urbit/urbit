source $stdenv/setup

set -eo pipefail

# File naming is intentional - xxd will generate the hex dump
# array and length names from the file name.
cat $src > u3_Ivory.pill

xxd -i u3_Ivory.pill > ivory.h

mkdir -p $out/include

mv ivory.h $out/include

rm u3_Ivory.pill
