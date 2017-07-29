source $setup

tar -xf $src
mv libxcb-* libxcb

mkdir build
cd build

../libxcb/configure --prefix=$out $configure_flags
# cat config.log

make

make install
