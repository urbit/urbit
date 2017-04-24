source $setup

# This is from the mingw-w64-readline AUR arch package.
export bash_cv_wcwidth_broken=no

tar -xf $src

cd readline-$version
for patch in $patches_p2; do
  echo applying patch $patch
  patch -p2 -i $patch
done
for patch in $patches; do
  echo applying patch $patch
  patch -p1 -i $patch
done
cd ..

mkdir build
cd build

../readline-$version/configure \
  --prefix=$out --host=$host \
  --enable-static --disable-shared \
  --with-curses=$curses

make

make install

mkdir $out/license
cp ../readline-$version/COPYING $out/license/LICENSE
