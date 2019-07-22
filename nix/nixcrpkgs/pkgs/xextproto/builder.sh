source $setup

tar -xf $src
mv xextproto-* xextproto

mkdir build
cd build

../xextproto/configure --prefix=$out

make

make install
