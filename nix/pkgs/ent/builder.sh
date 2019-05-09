source $stdenv/setup

cp -r $src ./src
chmod -R u+w ./src
cd ./src

bash ./configure
PREFIX=$out make install
