source $stdenv/setup

cp -r $src ./src
chmod -R u+w ./src
cd src

libtoolize
bash ./autogen.sh
bash ./configure --prefix=$out --host=$host $configureFlags
make
make install
