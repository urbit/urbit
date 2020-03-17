source $setup

tar -xf $src
mv libusb-* libusb

mkdir build
cd build

if [ -n "$libudev" ]; then
  export CFLAGS="${CFLAGS:=} -isystem $libudev/include"
  export LDFLAGS="${LDFLAGS:=} -L$libudev/lib"
fi

../libusb/configure \
  --prefix=$out \
  --host=$host \
  --enable-static \
  --disable-shared

make

make install

if [ -n "$libudev" ]; then
  ln -s $libudev/lib/pkgconfig/*.pc $out/lib/pkgconfig/
  echo "Requires: libudev" >> $out/lib/pkgconfig/libusb-1.0.pc
fi

# Make static linking work
sed -i 's/Libs.private/Libs/' $out/lib/pkgconfig/*.pc
