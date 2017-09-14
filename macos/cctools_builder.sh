source $setup

tar -xf $src
mv cctools-port-* cctools-port
mv cctools-port/cctools .
mv cctools-port/tools .
# sh tools/fix_unistd_issue.sh

mkdir build
cd build

../cctools/configure --prefix=$out $configure_flags

make

make install
