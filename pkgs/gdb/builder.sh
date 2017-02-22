source $stdenv/setup

unset CC
unset CXX
unset LD

tar -xf $src

mkdir build
cd build

# TODO: enable the text UI some day
../gdb-$version/configure \
  --prefix=$out \
  --host=$host \
  --target=$host \
  --with-zlib=$zlib \
  --with-expat=$expat \
  --with-readline=$readline \
  --with-curses=$pdcurses \
  --disable-tui \
  --disable-win32-registry \
  --disable-rpath

make

make install
