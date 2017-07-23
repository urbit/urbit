source $setup

tar -xf $src
mv libusbp-* libusbp

mkdir build
cd build

$host-g++ ../libusbp/examples/lsusb/*.cpp -o lsusb$exe_suffix \
  $(pkg-config-cross --cflags --libs libusbp-1)

mkdir -p $out/bin
cp lsusb$exe_suffix $out/bin/
