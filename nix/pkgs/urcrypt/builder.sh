source $stdenv/setup

cp -r $src ./src
chmod -R u+w ./src
cd src

./autogen.sh
./configure --prefix=$out $configureFlags
make
make install
