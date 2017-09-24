source $setup

tar -xf $src
mv cctools-port-* cctools-port

cd cctools-port/cctools
sh autogen.sh
cd ../..

mkdir build
cd build

../cctools-port/cctools/configure --prefix=$out $configure_flags

make

make install
