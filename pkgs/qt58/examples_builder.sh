source $stdenv/setup

tar -xf $src qtbase-opensource-src-$version/examples

mv qtbase-opensource-src-$version/examples .
rmdir qtbase-opensource-src-$version

mkdir build
cd build

# TODO: use $host-pkg-config to get the compiler settings

echo "compiling rasterwindow"
$host-g++ \
  -I $qtbase/include/ \
  -I $qtbase/include/QtGui \
  -L $qtbase/lib \
  ../examples/gui/rasterwindow/*.cpp \
  -lQt5Gui -lQt5Core \
  -lmpr -lwinmm -lqtpcre -lqtlibpng -lqtharfbuzz \
  -lole32 -luuid -lwinmm -lws2_32 \
  -o rasterwindow${exe_suffix}

