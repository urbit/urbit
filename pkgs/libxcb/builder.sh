source $setup

tar -xf $src
mv libxcb-* libxcb

mkdir build
cd build

autoreconf -vfi

PKG_CONFIG=pkg-config-cross \
../libxcb/configure --prefix=$out $configure_flags
# cat config.log

make

make install
