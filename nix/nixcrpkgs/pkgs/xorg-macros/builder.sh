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

# xorg-macros.m4 uses AC_REQUIRE to pull in PKG_PROG_PKG_CONFIG, so pkg.m4
# needs to be on the ACLOCAL_PATH.
ln -s $pkgconfig/share/aclocal/* $out/share/aclocal/
