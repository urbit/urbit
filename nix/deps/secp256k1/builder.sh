source $stdenv/setup

cp -r $src ./src
chmod -R u+w ./src
cd src

libtoolize
./autogen.sh
./configure --prefix=$out --host=$host $configureFlags
make
make install
