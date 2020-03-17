source $setup

tar -xf $src
ls
mv xcb-proto-* xcb-proto

mkdir build
cd build

../xcb-proto/configure --prefix=$out

make

make install
