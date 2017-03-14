source $stdenv/setup

tar -xf $src

ls

cd binutils-$version
for patch in $patches; do
  echo applying patch $patch
  patch -p1 -i $patch
done

# Clear the default library search path (noSysDirs)
echo 'NATIVE_LIB_DIRS=' >> ld/configure.tgt

# Use symlinks instead of hard links to save space ("strip" in the
# fixup phase strips each hard link separately).
for i in binutils/Makefile.in gas/Makefile.in ld/Makefile.in gold/Makefile.in; do
  sed -i "$i" -e 's|ln |ln -s |'
done

cd ..

mkdir build
cd build

../binutils-$version/configure --prefix=$out $configureFlags

make

make install

