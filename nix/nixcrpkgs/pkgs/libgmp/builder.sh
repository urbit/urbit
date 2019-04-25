source $setup

tar -xf $src

mkdir build
cd build
../gmp-$version/configure --host=$host --prefix=$out --disable-shared
make
make install
