source $stdenv/setup

cp -r $src ./src
chmod -R u+w ./src
cd ./src

./configure
PREFIX=$out make install
