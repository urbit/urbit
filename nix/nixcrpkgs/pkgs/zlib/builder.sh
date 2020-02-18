source $setup

tar -xf $src

mkdir build
cd build

../zlib-$version/configure --prefix=$out --static

make

make install
