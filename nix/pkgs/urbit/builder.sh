source $stdenv/setup

cp -r $src ./src
chmod -R u+w ./src
cd src

bash ./configure

make clean
make urbit urbit-worker -j8
make test

mkdir -p $out/bin
cp urbit $out/bin/$exename
cp urbit-worker $out/bin/$exename-worker
