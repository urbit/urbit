source $setup

tar -xf $src
mv inputproto-* proto

mkdir build
cd build

../proto/configure --prefix=$out

make

make install
