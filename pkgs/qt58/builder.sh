source $stdenv/setup

time tar -xf $src

mkdir build
cd build

../qt-everywhere-opensource-src-$version/configure \
  -opensource \
  -confirm-license \
  -release \
  -nomake examples
