source $stdenv/setup
unset CC CXX AR LD

tar -xf $src

mv pololu-usb-avr-programmer-v2-$version pavr2-$version

mkdir build
cd build

cmake ../pavr2-$version \
  -DCMAKE_TOOLCHAIN_FILE=$cmake_toolchain \
  -DCMAKE_INSTALL_PREFIX=$out \
  -DENABLE_GUI=false   # TODO: enable the GUI

make

make install
