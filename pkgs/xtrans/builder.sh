source $setup

tar -xf $src
mv xtrans-* xtrans

mkdir build
cd build

../xtrans/configure --prefix $out

make

make install

# So we can find the pkgconfig files in lib/pkgconfig
ln -s share $out/lib
