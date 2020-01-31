source $stdenv/setup

cp -r $src ./src
chmod -R u+w ./src
cd src

bash ./configure

make clean
make all -j8
make test

mkdir -p $out/bin
cp ./build/urbit $out/bin/$exename
cp ./build/urbit-worker $out/bin/$exename-worker
