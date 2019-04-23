source $setup

tar -xf $src
mv xproto-* xproto

cp $gnu_config/{config.guess,config.sub} xproto

mkdir build
cd build

PKG_CONFIG=pkg-config-cross \
../xproto/configure --prefix=$out $configure_flags

make

make install
