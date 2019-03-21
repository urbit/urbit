source $stdenv/setup

cp -r $src ./src
chmod -R a+w ./src
cd ./src

LIBTOOLIZE=libtoolize ./autogen.sh
bash ./configure --prefix=$out --host=$host $configureFlags
make install
