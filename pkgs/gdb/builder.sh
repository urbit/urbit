source $stdenv/setup

unset CC
unset LD

tar -xf $src

mkdir build
cd build

../gdb-$version/configure \
  --host=$host \
  --target=$host \
  --with-zlib=$zlib \
  --with-expat=$expat \
  --with-readline=$readline \
  --with-curses=$pdcurses \
  --disable-tui \  # TODO: enable the text UI some day
  --disable-win32-registry \
  --disable-rpath

make

make install
