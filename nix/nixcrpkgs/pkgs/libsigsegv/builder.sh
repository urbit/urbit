source $setup

tar -xf $src

mkdir build
cd build

# Hack
if [ $host = aarch64-linux-musleabi ]
then
  sed -i 's/^CFG_FAULT=$/CFG_FAULT=fault-linux-arm.h/' \
    ../libsigsegv-$version/configure
fi

../libsigsegv-$version/configure \
  --host=$host \
  --prefix=$out \
  --enable-static=yes \
  --enable-shared=no

make
make install
