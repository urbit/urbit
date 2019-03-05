source $stdenv/setup

cp -r $src ./src
chmod -R a+w ./src
cd ./src

LIBTOOLIZE=libtoolize ./autogen.sh
./configure --prefix=$out --host=$host $configureFlags
make install
