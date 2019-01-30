source $setup

cp -r $src openocd
chmod -R u+w openocd

cd openocd
for patch in $patches; do
  echo applying patch $patch
  patch -p1 -i $patch
done
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

$host-strip $out/bin/openocd$exe_suffix
