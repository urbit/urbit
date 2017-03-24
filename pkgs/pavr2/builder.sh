source $stdenv/setup
unset CC CXX AR LD

tar -xf $src

mv pololu-usb-avr-programmer-v2-$version pavr2-$version

mkdir build
cd build

# TODO: these CXXFLAGS and LDFLAGS should really come form libusbp-1.pc
export CXXFLAGS="-DLIBUSBP_STATIC"
export LIBS="-lsetupapi -lwinusb -luuid -lole32"

cmake ../pavr2-$version \
  -DCMAKE_TOOLCHAIN_FILE=$cmake_toolchain \
  -DCMAKE_INSTALL_PREFIX=$out \
  -DCMAKE_CXX_STANDARD_LIBRARIES="$LIBS" \
  -DENABLE_GUI=false   # TODO: enable the GUI

make

make install
