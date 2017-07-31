source $setup

tar -xf $src
mv kbproto-* proto

mkdir build
cd build

../proto/configure --prefix=$out

make

make install
