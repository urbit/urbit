source $stdenv/setup

time tar -xf $src

cd qt-everywhere-opensource-src-$version
# TODO: patch qt to not use /bin/pwd, test building it in a sandbox
cd ..

mkdir build
cd build

../qt-everywhere-opensource-src-$version/configure \
  -prefix $out \
  -device-option CROSS_COMPILE=${host}- \
  -opensource -confirm-license \
  -release \
  -static \
  -nomake examples \
  -opengl desktop \
  -xplatform win32-g++ \
  -no-icu

# TODO: maybe get it to use system zlib/libpng/libjpeg/FreeType/HarfBuzz/SQLite/pcre

make

cd ..
