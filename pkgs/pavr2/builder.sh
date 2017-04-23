source $setup

tar -xf $src
mv pololu-usb-avr-programmer-v2-* pavr2

mkdir build
cd build

PKG_CONFIG=$host-pkg-config \
cmake ../pavr2 \
  -DCMAKE_TOOLCHAIN_FILE=$cmake_toolchain \
  -DCMAKE_INSTALL_PREFIX=$out \
  -DENABLE_GUI=false   # TODO: enable the GUI

make

make install
