source $stdenv/setup

cp -r $src ./src
chmod -R u+w ./src
cd src

bash ./configure

make urbit-worker -j8

mkdir -p $out/bin
cp urbit-worker $out/bin/$exename-worker
