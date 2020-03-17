source $setup

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

export LDFLAGS="-L$curses/lib"
export CFLAGS="-I$curses/include"
export CXXFLAGS="-I$curses/include"

../gdb-$version/configure \
  --prefix=$out \
  --host=$host \
  --target=$host \
  --with-expat=yes --with-libexpat-prefix=$expat \
  --enable-tui \
  --disable-win32-registry \
  --disable-rpath

make

make install
