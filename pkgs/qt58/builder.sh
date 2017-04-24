source $setup

tar -xf $src

cd qtbase-opensource-src-$version
for patch in $patches; do
  echo applying patch $patch
  patch -p1 -i $patch
done
cd ..

mkdir build
cd build

../qtbase-opensource-src-$version/configure -prefix $out $configure_flags

# TODO: maybe get it to use system zlib/libpng/libjpeg/FreeType/HarfBuzz/SQLite/pcre

make

make install
