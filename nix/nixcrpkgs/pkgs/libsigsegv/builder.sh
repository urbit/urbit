source $setup

tar -xf $src

mkdir build
cd build

# Hack to get this building on ARM
if [ $host = armv6-linux-musleabi ]
then
  sed -i 's/fault-linux-arm-old.h/fault-linux-arm.h/' \
    ../libsigsegv-$version/configure
fi

../libsigsegv-$version/configure \
  --host=$host \
  --prefix=$out \
  --enable-static=yes \
  --enable-shared=no

make
make install
