source $setup

tar -xf $src
ls
mv util-macros-* macros

mkdir build
cd build

../macros/configure --prefix=$out

make

make install

# The .pc files gets installed to /share/pkgconfig, but we want to see it in
# /lib/pkgconfig.
ln -s share $out/lib
