source $stdenv/setup

unset CC CXX LD

tar -xf $src

mkdir build
cd build

../ncurses-$version/configure \
  --prefix=$out \
  --host=$host \
  --without-shared \
  --disable-rpath

make

make install
