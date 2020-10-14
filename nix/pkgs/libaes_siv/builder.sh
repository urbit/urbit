source $stdenv/setup

cp -r $src ./src
chmod -R u+w ./src
cd ./src

PREFIX=$out make install
