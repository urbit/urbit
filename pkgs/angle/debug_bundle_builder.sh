source $stdenv/setup

mkdir -p $out

cp -r $gdb/* $out
chmod -R u+w $out

cp -r $examples/* $out
chmod -R u+w $out

mkdir -p $out/src
cp -r $src/* $out/src/
chmod -R u+w $out
