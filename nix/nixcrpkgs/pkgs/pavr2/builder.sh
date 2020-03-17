source $setup

tar -xf $src
mv pololu-usb-avr-programmer-v2-* pavr2

mkdir build
cd build

cmake-cross ../pavr2 \
  -DCMAKE_INSTALL_PREFIX=$out

make

make install
