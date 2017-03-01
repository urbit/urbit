source $stdenv/setup

unset CC
unset CXX
unset LD

tar -xf $src

cd gdb-$version
for patch in $patches
do
  echo applying patch $patch
  patch -p1 -i $patch
done
cd ..

mkdir build
cd build

# TODO: enable the text UI some day
../gdb-$version/configure \
  --prefix=$out \
  --host=$host \
  --target=$host \
  --with-zlib=$zlib \
  --with-expat=$expat \
  --with-curses=$curses \
  --disable-tui \
  --disable-win32-registry \
  --disable-rpath

make

make install
