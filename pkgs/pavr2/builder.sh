source $stdenv/setup

tar -xf $src

mv pololu-usb-avr-programmer-v2-$version pavr2-$version

mkdir build
cd build

cmake ../pavr2-$version \
  -DCMAKE_TOOLCHAIN_FILE=$cmake_toolchain \
  -DCMAKE_INSTALL_PREFIX=$out

make

make install
