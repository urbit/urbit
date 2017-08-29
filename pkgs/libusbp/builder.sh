source $setup

tar -xf $src
mv libusbp-* libusbp

mkdir build
cd build

cmake-cross ../libusbp \
  -DCMAKE_INSTALL_PREFIX=$out \
  -DBUILD_SHARED_LIBS=false

make

make install

if [ -d $out/bin ]; then
  find $out/bin -type f -exec $host-strip {} +
fi

if [ -n "$libudev" ]; then
  ln -s $libudev/lib/pkgconfig/*.pc $out/lib/pkgconfig/
fi
