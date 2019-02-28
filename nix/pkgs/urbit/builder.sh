source $stdenv/setup

cp -r $src ./src
chmod -R u+w ./src
cd src

./configure

make urbit urbit-worker -j8

mkdir -p $out/bin
cp urbit $out/bin/$exename
cp urbit-worker $out/bin/$exename-worker
