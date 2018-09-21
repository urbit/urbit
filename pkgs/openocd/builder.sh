source $setup

cp -r $src openocd
chmod -R u+w openocd

cd openocd
SKIP_SUBMODULE=1 ./bootstrap
cd ..

mkdir build
cd build

PKG_CONFIG=pkg-config-cross ../openocd/configure \
  --prefix=$out \
  --host=$host \
  --disable-dependency-tracking \
  --enable-static \
  --disable-shared

make

make install

$host-strip $out/bin/openocd
