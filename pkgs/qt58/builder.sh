source $stdenv/setup

time tar -xf $src

mkdir build
cd build

../qt-everywhere-opensource-src-$version/configure \
  -opensource \
  -confirm-license \
  -xplatform win32-g++ \
  -device-option CROSS_COMPILE=${arch}- \
  -nomake examples \
  -release

