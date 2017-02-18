source $stdenv/setup

export CC="${host}-gcc"
export AR="${host}-ar"
export RANLIB="${host}-ranlib"

tar -xf $src

mkdir build
cd build

../zlib-$version/configure --prefix=$out --static

make

make install
