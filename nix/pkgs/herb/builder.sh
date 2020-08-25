source $stdenv/setup

set -eo pipefail

mkdir -p $out/bin

cp $src $out/bin/herb

chmod +x $out/bin/herb
