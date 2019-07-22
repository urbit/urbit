source $setup

tar -xf $src
cd *

./configure --host=$host --prefix=$out $configureFlags

make

make install.{libs,includes,data}

# TODO Why do I need to do this?
mkdir -p $out/lib/pkgconfig
cp misc/*.pc $out/lib/pkgconfig
