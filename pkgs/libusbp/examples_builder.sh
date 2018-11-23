source $setup

tar -xf $src
mv libusbp-* libusbp

mkdir build
cd build

FLAGS="-std=gnu++11 $(pkg-config-cross --cflags --libs libusbp-1)"

$host-g++ ../libusbp/examples/lsusb/*.cpp -o lsusb$exe_suffix $FLAGS
$host-g++ ../libusbp/examples/lsport/*.cpp -o lsport$exe_suffix $FLAGS

mkdir -p $out/bin
cp * $out/bin/
