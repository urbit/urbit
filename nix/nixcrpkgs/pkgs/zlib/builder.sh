source $setup

tar -xf $src

mkdir build
cd build

sed -i 's$Darwin. | darwin.$Ignore* | ignore*$' ../zlib-$version/configure

CHOST=$host \
../zlib-$version/configure --prefix=$out --static

make

make install
