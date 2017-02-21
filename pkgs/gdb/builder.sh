source $stdenv/setup

tar -xf $src

mkdir build
cd build

../gdb-$version/configure \
  --host=$host \
  --target=$host \
  --with-zlib=$zlib \
  --with-expat=$expat \
  --with-readline=$readline

make

make install
