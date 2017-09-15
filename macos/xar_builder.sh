source $setup

tar -xf $src
mv xar-* xar

mkdir build
cd build

export CFLAGS="$(pkg-config --cflags libcrypto zlib)"
export LDFLAGS="$(pkg-config --libs libcrypto zlib)"

../xar/configure --prefix=$out

make

make install
