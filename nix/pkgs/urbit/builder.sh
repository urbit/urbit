source $stdenv/setup

cp -r $src ./src
chmod -R u+w ./src
cd src

bash ./configure

make urbit -j8

mkdir -p $out/bin
cp urbit $out/bin/$exename
