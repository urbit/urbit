source $stdenv/setup

unset CC CXX CFLAGS LDFLAGS LD AR AS RANLIB SIZE STRINGS NM STRIP OBJCOPY

tar -xf $src

cd binutils-$version
for patch in $patches; do
  echo applying patch $patch
  patch -p1 -i $patch
done

# Clear the default library search path (noSysDirs)
echo 'NATIVE_LIB_DIRS=' >> ld/configure.tgt

cd ..

mkdir build
cd build

../binutils-$version/configure --prefix=$out $configure_flags

make

make install

